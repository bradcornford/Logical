<?php namespace Cornford\Logical;

use Cornford\Logical\Contracts\LogicalFactoryInterface;
use Cornford\Logical\Contracts\LogicalStatementInterface;
use Cornford\Logical\Logical;
use Cornford\Logical\LogicalStatement;

class LogicalFactory implements LogicalFactoryInterface
{
    /**
     * Build a new Logical object and its dependencies
     *
     * @param array       $input
     * @param string|null $logic
     *
     * @return \Cornford\Logical\Contracts\LogicalInterface
     */
    public function build(
        array $input = [],
        $logic = null
    ) {
        $logicalStatement = $this->buildLogicalStatement();

        return $this->buildLogical($input, $logic, $logicalStatement);
    }

    /**
     * Build the Logical object
     *
     * @param array                     $input
     * @param string|null               $logic
     * @param LogicalStatementInterface $logicStatement
     *
     * @return \Cornford\Logical\Contracts\LogicalInterface
     */
    public function buildLogical(
        array $input = [],
        $logic = null,
        LogicalStatementInterface $logicStatement
    ) {
        return new Logical($input, $logic, $logicStatement);
    }

    /**
     * Build a new Logical Statement object
     *
     * @return \Cornford\Logical\Contracts\LogicalStatementInterface
     */
    public function buildLogicalStatement()
    {
        return new LogicalStatement();
    }
}
