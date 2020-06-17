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

namespace ApiPlatform\SchemaGenerator\Model;

final class ClassStruct
{
    use StructTrait;
    public ?Interface_ $interface = null;
    public string $namespace = '';
    public array $uses = [];
    public array $constants = [];
    public array $properties = [];
    public bool $hasConstructor = false;
    public bool $parentHasConstructor = false;
    public bool $hasChild = false;
    public bool $abstract = false;
}

