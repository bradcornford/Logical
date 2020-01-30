<?php namespace Cornford\Logical\Facades;

use Illuminate\Support\Facades\Facade;

class LogicalFacade extends Facade
{
    protected static function getFacadeAccessor()
    {
        return 'logical';
    }
}
