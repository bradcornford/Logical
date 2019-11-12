<?php namespace Cornford\Logical\Contracts;

interface LogicalFactoryInterface
{
    /**
     * Build a new Logical object and its dependencies
     *
     * @param array       $input
     * @param string|null $logic
     *
     * @return \Cornford\Logical\Contracts\LogicalInterface
     */
    public function build(array $input = [], $logic = null);

    /**
     * Build the Logical object
     *
     * @param array                     $input
     * @param string|null               $logic
     * @param LogicalStatementInterface $logicStatement
     *
     * @return \Cornford\Logical\Contracts\LogicalInterface
     */
    public function buildLogical(array $input = [], $logic = null, LogicalStatementInterface $logicStatement);

    /**
     * Build a new Logical Statement object
     *
     * @return \Cornford\Logical\Contracts\LogicalStatementInterface
     */
    public function buildLogicalStatement();
}
