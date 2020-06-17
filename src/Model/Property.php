<?php
declare(strict_types=1);

namespace ApiPlatform\SchemaGenerator\Model;


final class Property
{
    use StructTrait;
    public bool $isArray = false;
    public array $getterAnnotations = [];
    public array $adderAnnotations = [];
    public array $removerAnnotations = [];
    public array $setterAnnotations = [];
}
