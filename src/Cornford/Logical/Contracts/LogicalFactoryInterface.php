<?php namespace Cornford\Logical\Contracts;

interface LogicalFactoryInterface {

	/**
	 * Build a new Logical object and its dependencies
	 *
	 * @param array       $input
	 * @param string|null $logic
	 *
	 * @return Logical
	 */
	public function build(
		array $input = [],
		$logic = null
	);

	/**
	 * Build the Logical object
	 *
	 * @param array                     $input
	 * @param string|null               $logic
	 * @param LogicalStatementInterface $logicStatement
	 *
	 * @return Logical
	 */
	public function buildLogical(
		array $input = [],
		$logic = null,
		LogicalStatementInterface $logicStatement
	);

	/**
	 * Build a new Logical Statement object
	 *
	 * @return LogicalStatement
	 */
	public function buildLogicalStatement();

}
