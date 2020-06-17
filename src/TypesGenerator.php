<?php

/*
 * This file is part of the API Platform project.
 *
 * (c) Kévin Dunglas <dunglas@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

declare(strict_types=1);

namespace ApiPlatform\SchemaGenerator;

use ApiPlatform\SchemaGenerator\AnnotationGenerator\AnnotationGeneratorInterface;
use Doctrine\Common\Collections\ArrayCollection;
use Doctrine\Common\Collections\Collection;
use Doctrine\Inflector\Inflector;
use MyCLabs\Enum\Enum;
use Nette\PhpGenerator\ClassType;
use Nette\PhpGenerator\PhpNamespace;
use PhpCsFixer\Cache\NullCacheManager;
use PhpCsFixer\Differ\NullDiffer;
use PhpCsFixer\Error\ErrorsManager;
use PhpCsFixer\FixerFactory;
use PhpCsFixer\Linter\Linter;
use PhpCsFixer\RuleSet;
use PhpCsFixer\Runner\Runner;
use Psr\Log\LoggerInterface;
use Symfony\Component\Filesystem\Filesystem;
use Twig\Environment;

/**
 * Generates entity files.
 *
 * @author Kévin Dunglas <dunglas@gmail.com>
 */
class TypesGenerator
{
    /**
     * @var string
     *
     * @internal
     */
    public const SCHEMA_ORG_ENUMERATION = 'http://schema.org/Enumeration';

    /**
     * @var string
     */
    private const SCHEMA_ORG_DOMAIN = 'schema:domainIncludes';

    /**
     * @var string
     */
    private const SCHEMA_ORG_RANGE = 'schema:rangeIncludes';

    /**
     * @var string
     */
    private const SCHEMA_ORG_SUPERSEDED_BY = 'schema:supersededBy';

    private Environment $twig;
    private LoggerInterface $logger;
    private array $graphs;
    private GoodRelationsBridge $goodRelationsBridge;
    private array $cardinalities;
    private Inflector $inflector;
    private Filesystem $filesystem;

    /**
     * @param \EasyRdf_Graph[] $graphs
     */
    public function __construct(Inflector $inflector, Environment $twig, LoggerInterface $logger, array $graphs, CardinalitiesExtractor $cardinalitiesExtractor, GoodRelationsBridge $goodRelationsBridge)
    {
        if (!$graphs) {
            throw new \InvalidArgumentException('At least one graph must be injected.');
        }

        $this->inflector = $inflector;
        $this->twig = $twig;
        $this->logger = $logger;
        $this->graphs = $graphs;
        $this->goodRelationsBridge = $goodRelationsBridge;
        $this->filesystem = new Filesystem();

        $this->cardinalities = $cardinalitiesExtractor->extract();
    }

    /**
     * Generates files.
     */
    public function generate(array $config): void
    {
        $baseClass = [
            'constants' => [],
            'fields' => [],
            'uses' => [],
            'hasConstructor' => false,
            'parentHasConstructor' => false,
            'hasChild' => false,
            'abstract' => false,
        ];

        $typesToGenerate = [];

        if (!$config['types']) {
            foreach ($this->graphs as $graph) {
                $typesToGenerate = $graph->allOfType('rdfs:Class');
            }
        } else {
            foreach ($config['types'] as $typeName => $typeConfig) {
                $resource = null;
                foreach ($this->graphs as $graph) {
                    $resources = $graph->resources();

                    if (isset($resources[$typeConfig['vocabularyNamespace'].$typeName])) {
                        $resource = $graph->resource($typeConfig['vocabularyNamespace'].$typeName, 'rdfs:Class');
                        break;
                    }
                }

                if ($resource) {
                    $typesToGenerate[$typeName] = $resource;
                } else {
                    $this->logger->warning('Type "{typeName}" cannot be found. Using "{guessFrom}" type to generate entity.', ['typeName' => $typeName, 'guessFrom' => $typeConfig['guessFrom']]);
                    if (isset($graph)) {
                        $type = $graph->resource($typeConfig['vocabularyNamespace'].$typeConfig['guessFrom'], 'rdfs:Class');
                        $typesToGenerate[$typeName] = $type;
                    }
                }
            }
        }

        $namespaces = [];
        $classes = [];
        $interfaces = [];

        $propertiesMap = $this->createPropertiesMap($typesToGenerate);

        foreach ($typesToGenerate as $typeName => $type) {
            $typeName = \is_string($typeName) ? $typeName : $type->localName();
            $typeConfig = $config['types'][$typeName] ?? null;

            $classMetadata = [];
            $classMetadata['isEnum'] = $this->isEnum($type);

            $class = new ClassType($typeName);
            $comment = $type->get('rdfs:comment');

            $classMetadata['label'] = $comment ? $comment->getValue() : '';
            $classMetadata['resource'] = $type;
            $classMetadata['config'] = $typeConfig;

            if ($classMetadata['isEnum']) {
                $class->setExtends(Enum::class);

                $namespaceName = $typeConfig['namespace'] ?? $config['namespaces']['enum'];
                if (!isset($namespaces[$namespaceName])) {
                    $namespaces[$namespaceName] = new PhpNamespace($namespaceName);
                }
                $namespaces[$namespaceName]->add($class);

                // Constants
                foreach ($this->graphs as $graph) {
                    foreach ($graph->allOfType($type->getUri()) as $instance) {
                        $class
                            ->addConstant(strtoupper(substr(preg_replace('/([A-Z])/', '_$1', $instance->localName()), 1)), $instance->getUri())
                            // TODO: add annotations
                        ;
                    }
                }
            } else {
                // Entities
                $namespaceName = $typeConfig['namespaces']['class'] ?? $config['namespaces']['entity'];
                if (!isset($namespaces[$namespaceName])) {
                    $namespaces[$namespaceName] = new PhpNamespace($namespaceName);
                }
                $namespaces[$namespaceName]->add($class);

                $parent = $typeConfig['parent'] ?? null;
                if (!$parent && $numberOfSupertypes = \count($type->all('rdfs:subClassOf'))) {
                    if ($numberOfSupertypes > 1) {
                        $this->logger->warning(sprintf('The type "%s" has several supertypes. Using the first one.', $type->localName()));
                    }

                    $parent = $type->all('rdfs:subClassOf')[0]->localName();
                    $class->setExtends(sprintf('%s\\%s', $config['types'][$parent]['namespaces']['class'] ?? '', $parent));
                }

                // Embeddable
                $classMetadata['embeddable'] = $typeConfig['embeddable'] ?? false;

                if ($config['types'] && $parent && !isset($config['types'][$parent])) {
                    $this->logger->error(sprintf('The type "%s" (parent of "%s") doesn\'t exist', $class['parent'], $type->localName()));
                }

                // Interfaces
                if ($config['useInterface']) {
                    $namespaceName = $typeConfig['namespaces']['interface'] ?? $config['namespaces']['interface'];
                    if (!isset($namespaces[$namespaceName])) {
                        $namespaces[$namespaceName] = new PhpNamespace($namespaceName);
                    }

                    $interface = new ClassType(sprintf('%sInterface', $typeName));
                    $interface->setInterface();
                    $namespaces[$namespaceName]->add($interface);

                    $class->setImplements(sprintf('%s\\%s', $namespaceName, $interface->getName()));
                }
            }

            // Fields
            if (!$typeConfig['allProperties'] && isset($typeConfig['properties']) && \is_array($typeConfig['properties'])) {
                foreach ($typeConfig['properties'] as $key => $value) {
                    foreach ($propertiesMap[$type->getUri()] as $property) {
                        if ($key !== $property->localName()) {
                            continue;
                        }

                        $class = $this->addProperty($config, $class, $type, $typeName, $property->localName(), $property);
                        continue 2;
                    }

                    // Add custom fields (non schema.org)
                    $this->logger->info(sprintf('The property "%s" (type "%s") is a custom property.', $key, $type->localName()));
                    $class = $this->addProperty($config, $class, $type, $typeName, $key);
                }
            } else {
                // All properties
                foreach ($propertiesMap[$type->getUri()] as $property) {
                    if ($property->hasProperty(self::SCHEMA_ORG_SUPERSEDED_BY)) {
                        $supersededBy = $property->get('schema:supersededBy');
                        $this->logger->warning(sprintf('The property "%s" is superseded by "%s". Using the superseding property.', $property->localName(), $supersededBy->localName()));
                    } else {
                        $class = $this->addProperty($config, $class, $type, $typeName, $property->localName(), $property);
                    }
                }
            }

            $classes[$typeName] = $class;
        }

        // Second pass
        foreach ($classes as &$class) {
            if ($class['parent'] && isset($classes[$class['parent']])) {
                $classes[$class['parent']]['hasChild'] = true;
                $class['parentHasConstructor'] = $classes[$class['parent']]['hasConstructor'];
            }

            foreach ($class['fields'] as &$field) {
                $field['isEnum'] = isset($classes[$field['range']]) && $classes[$field['range']]['isEnum'];
                $field['typeHint'] = $this->fieldToTypeHint($config, $field, $classes) ?? false;

                if ($field['isArray']) {
                    $field['adderRemoverTypeHint'] = $this->fieldToAdderRemoverTypeHint($field, $classes) ?? false;
                }
            }
        }

        // Third pass
        foreach ($classes as &$class) {
            $class['abstract'] = $config['types'][$class['name']]['abstract'] ?? $class['hasChild'];

            // When including all properties, ignore properties already set on parent
            if (isset($config['types'][$class['name']]['allProperties']) && $config['types'][$class['name']]['allProperties'] && isset($classes[$class['parent']])) {
                $type = $class['resource'];

                foreach ($propertiesMap[$type->getUri()] as $property) {
                    if (!isset($class['fields'][$property->localName()])) {
                        continue;
                    }

                    $parentConfig = $config['types'][$class['parent']] ?? null;
                    $parentClass = $classes[$class['parent']];

                    while ($parentClass) {
                        if (!isset($parentConfig['properties']) ||
                            !\is_array($parentConfig['properties']) ||
                            0 === \count($parentConfig['properties'])
                        ) {
                            // Unset implicit property
                            $parentType = $parentClass['resource'];
                            if (\in_array($property, $propertiesMap[$parentType->getUri()], true)) {
                                unset($class['fields'][$property->localName()]);
                                continue 2;
                            }
                        } else {
                            // Unset explicit property
                            if (\array_key_exists($property->localName(), $parentConfig['properties'])) {
                                unset($class['fields'][$property->localName()]);
                                continue 2;
                            }
                        }

                        $parentConfig = $parentClass['parent'] ? ($config['types'][$parentClass['parent']] ?? null) : null;
                        $parentClass = $parentClass['parent'] ? $classes[$parentClass['parent']] : null;
                    }
                }
            }
        }

        // Generate ID
        if ($config['id']['generate']) {
            foreach ($classes as &$class) {
                if ($class['hasChild'] || $class['isEnum'] || $class['embeddable']) {
                    continue;
                }

                switch ($config['id']['generationStrategy']) {
                    case 'auto':
                        $range = 'Integer';
                        $typeHint = 'int';
                        $writable = false;
                        $nullable = true;
                        break;
                    case 'uuid':
                        $range = 'Text';
                        $typeHint = 'string';
                        $writable = $config['id']['writable'];
                        $nullable = !$writable;
                        break;
                    case 'mongoid':
                        $range = 'Text';
                        $typeHint = 'string';
                        $writable = false;
                        $nullable = true;
                        break;
                    default:
                        $range = 'Text';
                        $typeHint = 'string';
                        $writable = true;
                        $nullable = false;
                        break;
                }

                $class['fields'] = [
                    'id' => [
                        'name' => 'id',
                        'resource' => null,
                        'range' => $range,
                        'cardinality' => CardinalitiesExtractor::CARDINALITY_1_1,
                        'ormColumn' => null,
                        'isArray' => false,
                        'isReadable' => true,
                        'isWritable' => $writable,
                        'isNullable' => $nullable,
                        'isUnique' => false,
                        'isCustom' => true,
                        'isEnum' => false,
                        'isId' => true,
                        'typeHint' => $typeHint,
                    ],
                ] + $class['fields'];
            }
        }

        // Initialize annotation generators
        $annotationGenerators = [];
        foreach ($config['annotationGenerators'] as $annotationGenerator) {
            $generator = new $annotationGenerator($this->inflector, $this->logger, $this->graphs, $this->cardinalities, $config, $classes);

            $annotationGenerators[] = $generator;
        }

        $interfaceMappings = [];
        $generatedFiles = [];

        foreach ($classes as $className => &$class) {
            $class['uses'] = $this->generateClassUses($annotationGenerators, $classes, $className);
            $class['annotations'] = $this->generateClassAnnotations($annotationGenerators, $className);
            if (false === isset($typesToGenerate[$className])) {
                $class['interfaceAnnotations'] = $this->generateInterfaceAnnotations($annotationGenerators, $className);
            }

            foreach ($class['constants'] as $constantName => $constant) {
                $class['constants'][$constantName]['annotations'] = $this->generateConstantAnnotations($annotationGenerators, $className, $constantName);
            }

            foreach ($class['fields'] as $fieldName => &$field) {
                $field['annotations'] = $this->generateFieldAnnotations($annotationGenerators, $className, $fieldName);
                $field['getterAnnotations'] = $this->generateGetterAnnotations($annotationGenerators, $className, $fieldName);

                if ($field['isArray']) {
                    $field['adderAnnotations'] = $this->generateAdderAnnotations($annotationGenerators, $className, $fieldName);
                    $field['removerAnnotations'] = $this->generateRemoverAnnotations($annotationGenerators, $className, $fieldName);
                } else {
                    $field['setterAnnotations'] = $this->generateSetterAnnotations($annotationGenerators, $className, $fieldName);
                }
            }

            $classDir = $this->namespaceToDir($config, $class['namespace']);
            $this->filesystem->mkdir($classDir);

            $path = sprintf('%s%s.php', $classDir, $className);
            $generatedFiles[] = $path;

            file_put_contents(
                $path,
                $this->twig->render('class.php.twig', [
                    'config' => $config,
                    'class' => $class,
                ])
            );

            if (isset($class['interfaceNamespace'])) {
                $interfaceDir = $this->namespaceToDir($config, $class['interfaceNamespace']);
                $this->filesystem->mkdir($interfaceDir);

                $path = sprintf('%s%s.php', $interfaceDir, $class['interfaceName']);
                $generatedFiles[] = $path;
                file_put_contents(
                    $path,
                    $this->twig->render('interface.php.twig', [
                        'config' => $config,
                        'class' => $class,
                    ])
                );

                if ($config['doctrine']['resolveTargetEntityConfigPath'] && !$class['abstract']) {
                    $interfaceMappings[$class['interfaceNamespace'].'\\'.$class['interfaceName']] = $class['namespace'].'\\'.$className;
                }
            }
        }

        if (\count($interfaceMappings) > 0 && $config['doctrine']['resolveTargetEntityConfigPath']) {
            $file = $config['output'].'/'.$config['doctrine']['resolveTargetEntityConfigPath'];
            $dir = \dirname($file);
            $this->filesystem->mkdir($dir);

            file_put_contents(
                $file,
                $this->twig->render('doctrine.xml.twig', ['mappings' => $interfaceMappings])
            );

            $generatedFiles[] = $file;
        }

        $this->fixCs($generatedFiles);
    }

    /**
     * Tests if a type is an enum.
     */
    private function isEnum(\EasyRdf_Resource $type): bool
    {
        $subClassOf = $type->get('rdfs:subClassOf');

        return $subClassOf && self::SCHEMA_ORG_ENUMERATION === $subClassOf->getUri();
    }

    /**
     * Gets the parent classes of the current one and add them to $parentClasses array.
     *
     * @param string[] $parentClasses
     */
    private function getParentClasses(\EasyRdf_Resource $resource, array $parentClasses = []): array
    {
        if ([] === $parentClasses) {
            return $this->getParentClasses($resource, [$resource->getUri()]);
        }

        $subclasses = $resource->all('rdfs:subClassOf');

        if (empty($subclasses)) {
            return $parentClasses;
        }

        $parentClass = $subclasses[0];
        $parentClasses[] = $parentClass->getUri();

        foreach ($this->graphs as $graph) {
            foreach ($graph->allOfType('rdfs:Class') as $type) {
                if ($type->getUri() === $parentClass->getUri()) {
                    $parentClasses = $this->getParentClasses($type, $parentClasses);

                    break 2;
                }
            }
        }

        return $parentClasses;
    }

    /**
     * Creates a map between classes and properties.
     */
    private function createPropertiesMap(array $types): array
    {
        $typesAsString = [];
        $map = [];
        foreach ($types as $type) {
            // get all parent classes until the root
            $parentClasses = $this->getParentClasses($type);
            $typesAsString[] = $parentClasses;
            $map[$type->getUri()] = [];
        }

        foreach ($this->graphs as $graph) {
            foreach ($graph->allOfType('rdf:Property') as $property) {
                foreach ($property->all(self::SCHEMA_ORG_DOMAIN) as $domain) {
                    foreach ($typesAsString as $typesAsStringItem) {
                        if (\in_array($domain->getUri(), $typesAsStringItem, true)) {
                            $map[$typesAsStringItem[0]][] = $property;
                        }
                    }
                }
            }
        }

        return $map;
    }

    /**
     * Is this type a datatype?
     */
    private function isDatatype(string $type): bool
    {
        return \in_array($type, ['Boolean', 'DataType', 'Date', 'DateTime', 'Float', 'Integer', 'Number', 'Text', 'Time', 'URL'], true);
    }

    private function fieldToTypeHint(array $config, array $field, array $classes): ?string
    {
        if ($field['isArray']) {
            return $config['doctrine']['useCollection'] ? 'Collection' : 'array';
        }

        return $this->fieldToAdderRemoverTypeHint($field, $classes);
    }

    private function fieldToAdderRemoverTypeHint(array $field, array $classes): ?string
    {
        if ($field['isEnum']) {
            return 'string';
        }

        switch ($field['range']) {
            case 'Boolean':
                return 'bool';
            case 'Float':
                return 'float';
            case 'Integer':
                return 'int';
            case 'Text':
            case 'URL':
                return 'string';
            case 'Date':
            case 'DateTime':
            case 'Time':
                return '\\'.\DateTimeInterface::class;
            case 'DataType':
            case 'Number':
                return null;
        }

        return $classes[$field['range']]['interfaceName'] ?? $classes[$field['range']]['name'];
    }

    /**
     * Updates generated $class with given property config.
     */
    private function addProperty(array $config, ClassType $class, \EasyRdf_Resource $type, string $typeName, string $propertyName, ?\EasyRdf_Resource $property = null): void
    {
        $typeConfig = $config['types'][$typeName] ?? null;
        $typeDefined = !empty($config['types']);

        // Warn when property are not part of GoodRelations
        if ($config['checkIsGoodRelations'] && !$this->goodRelationsBridge->exist($propertyName)) {
            $this->logger->warning(sprintf('The property "%s" (type "%s") is not part of GoodRelations.', $propertyName, $type->localName()));
        }

        // Ignore or warn when properties are legacy
        if ($property && preg_match('/legacy spelling/', (string) $property->get('rdfs:comment'))) {
            if (!isset($typeConfig['properties'])) {
                $this->logger->debug(sprintf('The property "%s" (type "%s") is legacy. Ignoring.', $propertyName, $type->localName()));

                return;
            }

            $this->logger->warning(sprintf('The property "%s" (type "%s") is legacy.', $propertyName, $type->localName()));
        }

        $propertyConfig = $typeConfig['properties'][$propertyName] ?? null;

        $ranges = [];
        if ($propertyConfig['range'] ?? false) {
            $ranges[] = $propertyConfig['range'];
        } elseif ($property) {
            foreach ($property->all(self::SCHEMA_ORG_RANGE) as $range) {
                $localName = $range->localName();
                if (!$typeDefined || isset($config['types'][$localName]) || $this->isDatatype($localName)) {
                    $ranges[] = $localName;
                }
            }
        }

        $localName = $type->localName();
        $numberOfRanges = \count($ranges);
        if (0 === $numberOfRanges) {
            $this->logger->error(sprintf('The property "%s" (type "%s") has an unknown type. Add its type to the config file.', $propertyName, $localName));
        } else {
            if ($numberOfRanges > 1) {
                $this->logger->warning(sprintf('The property "%s" (type "%s") has several types. Using the first one ("%s") or possible options("%s").', $propertyName, $localName, reset($ranges), implode('", "', $ranges)));
            }

            $cardinality = $propertyConfig['cardinality'] ?? false;
            if (!$cardinality || CardinalitiesExtractor::CARDINALITY_UNKNOWN === $cardinality) {
                $cardinality = $property ? $this->cardinalities[$propertyName] : CardinalitiesExtractor::CARDINALITY_1_1;
            }

            $isArray = \in_array($cardinality, [
                CardinalitiesExtractor::CARDINALITY_0_N,
                CardinalitiesExtractor::CARDINALITY_1_N,
                CardinalitiesExtractor::CARDINALITY_N_N,
            ], true);

            $isNullable = (bool) ($propertyConfig['nullable'] ?? !\in_array($cardinality, [
                    CardinalitiesExtractor::CARDINALITY_1_1,
                    CardinalitiesExtractor::CARDINALITY_1_N,
                ], true));

            $columnPrefix = false;
            $isEmbedded = $propertyConfig['embedded'] ?? false;

            if (true === $isEmbedded) {
                $columnPrefix = $propertyConfig['columnPrefix'] ?? false;
            }

            $class->addProperty($this->getPropertyName($propertyName, $isArray));

            $class['fields'][$propertyName] = [
                'name' => ,
                'resource' => $property,
                'range' => $ranges[0],
                'cardinality' => $cardinality,
                'ormColumn' => $propertyConfig['ormColumn'] ?? null,
                'isArray' => $isArray,
                'isReadable' => $propertyConfig['readable'] ?? true,
                'isWritable' => $propertyConfig['writable'] ?? true,
                'isNullable' => $isNullable,
                'isUnique' => isset($propertyConfig['unique']) && $propertyConfig['unique'],
                'isCustom' => empty($property),
                'isEmbedded' => $isEmbedded,
                'columnPrefix' => $columnPrefix,
                'mappedBy' => $propertyConfig['mappedBy'] ?? null,
                'inversedBy' => $propertyConfig['inversedBy'] ?? null,
                'isId' => false,
            ];

            if ($isArray) {
                $class['hasConstructor'] = true;

                if ($config['doctrine']['useCollection'] && !\in_array(ArrayCollection::class, $class['uses'], true)) {
                    $class['uses'][] = ArrayCollection::class;
                    $class['uses'][] = Collection::class;
                }
            }
        }
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateFieldAnnotations(array $annotationGenerators, string $className, string $fieldName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateFieldAnnotations($className, $fieldName);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateConstantAnnotations(array $annotationGenerators, string $className, string $constantName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateConstantAnnotations($className, $constantName);
        }

        return  array_merge(...$annotations);;
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateClassAnnotations(array $annotationGenerators, string $className): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateClassAnnotations($className);
        }

        return  array_merge(...$annotations);;
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateInterfaceAnnotations(array $annotationGenerators, string $className): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateInterfaceAnnotations($className);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateGetterAnnotations(array $annotationGenerators, string $className, string $fieldName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateGetterAnnotations($className, $fieldName);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateAdderAnnotations(array $annotationGenerators, string $className, string $fieldName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateAdderAnnotations($className, $fieldName);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateRemoverAnnotations(array $annotationGenerators, string $className, string $fieldName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations[] = $generator->generateRemoverAnnotations($className, $fieldName);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateSetterAnnotations(array $annotationGenerators, string $className, string $fieldName): array
    {
        $annotations = [];
        foreach ($annotationGenerators as $generator) {
            $annotations = $generator->generateSetterAnnotations($className, $fieldName);
        }

        return array_merge(...$annotations);
    }

    /**
     * @param AnnotationGeneratorInterface[] $annotationGenerators
     */
    private function generateClassUses(array $annotationGenerators, array $classes, string $className): array
    {
        $uses = $classes[$className]['uses'];

        if (isset($classes[$className]['interfaceNamespace'])
            && $classes[$className]['interfaceNamespace'] !== $classes[$className]['namespace']
        ) {
            $uses[] = sprintf(
                '%s\\%s',
                $classes[$className]['interfaceNamespace'],
                $classes[$className]['interfaceName']
            );
        }

        foreach ($classes[$className]['fields'] as $field) {
            if (isset($classes[$field['range']]['interfaceName'])) {
                $use = sprintf(
                    '%s\\%s',
                    $classes[$field['range']]['interfaceNamespace'],
                    $classes[$field['range']]['interfaceName']
                );

                if (!\in_array($use, $uses, true)) {
                    $uses[] = $use;
                }
            }
        }

        $classUses = [];
        foreach ($annotationGenerators as $generator) {
            $classUses[] = $generator->generateUses($className);
        }
        $uses = array_merge($uses, ...$classUses);

        // Order alphabetically
        sort($uses);

        return $uses;
    }

    /**
     * Converts a namespace to a directory path according to PSR-4.
     */
    private function namespaceToDir(array $config, string $namespace): string
    {
        if (null !== ($prefix = $config['namespaces']['prefix'] ?? null) && 0 === strpos($namespace, $prefix)) {
            $namespace = substr($namespace, \strlen($prefix));
        }

        return sprintf('%s/%s/', $config['output'], str_replace('\\', '/', $namespace));
    }

    /**
     * Uses PHP CS Fixer to make generated files following PSR and Symfony Coding Standards.
     */
    private function fixCs(array $files): void
    {
        $fileInfos = [];
        foreach ($files as $file) {
            $fileInfos[] = new \SplFileInfo($file);
        }

        $fixers = (new FixerFactory())
            ->registerBuiltInFixers()
            ->useRuleSet(new RuleSet([
                '@Symfony' => true,
                'array_syntax' => ['syntax' => 'short'],
                'phpdoc_order' => true,
                'declare_strict_types' => true,
            ]))
            ->getFixers();

        $runner = new Runner(
            new \ArrayIterator($fileInfos),
            $fixers,
            new NullDiffer(),
            null,
            new ErrorsManager(),
            new Linter(),
            false,
            new NullCacheManager()
        );
        $runner->fix();
    }

    private function getPropertyName(string $propertyName, bool $isArray): string
    {
        $snakeProperty = preg_replace('/([A-Z])/', '_$1', $propertyName);
        $exploded = explode('_', $snakeProperty);

        if (2 < \strlen($word = $exploded[\count($exploded) - 1])) {
            $exploded[\count($exploded) - 1] = $isArray ? $this->inflector->pluralize($word) : $this->inflector->singularize($word);

            return implode('', $exploded);
        }

        return $propertyName;
    }
}
