<?php namespace Cornford\Logical;

use Cornford\Logical\Contracts\LogicalFactoryInterface;
use Cornford\Logical\Contracts\LogicalStatementInterface;
use Cornford\Logical\Logical;
use Cornford\Logical\LogicalStatement;

class LogicalFactory implements LogicalFactoryInterface {

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
	 * @return Logical
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
	 * @return LogicalStatement
	 */
	public function buildLogicalStatement()
	{
		return new LogicalStatement();
	}

}
  