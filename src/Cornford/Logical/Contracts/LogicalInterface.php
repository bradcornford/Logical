<?php namespace Cornford\Logical\Contracts;

interface LogicalInterface {

	/**
	 * Construct Logical object
	 *
	 * @param array                     $input
	 * @param string|null               $logic
	 * @param LogicalStatementInterface $logicalStatement
	 *
	 * @return self
	 */
	public function __construct(
		array $input = [],
		$logic = null,
		LogicalStatementInterface $logicalStatement
	);

	/**
	 * Set the logical statement instance
	 *
	 * @param LogicalStatementInterface $value
	 *
	 * @return void
	 */
	public function setLogicalStatementInstance(LogicalStatementInterface $value);

	/**
	 * Get the logic statement instance
	 *
	 * @return LogicalStatementInterface
	 */
	public function getLogicalStatementInstance();

	/**
	 * Set the input value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	public function setInput(array $value = []);

	/**
	 * Get the input value
	 *
	 * @return array
	 */
	public function getInput();

	/**
	 * Set the logic value
	 *
	 * @param string $value
	 *
	 * @return void
	 */
	public function setLogic($value);

	/**
	 * Get the logic value
	 *
	 * @return array
	 */
	public function getLogic();


	/**
	 * Get the results value
	 *
	 * @return array
	 */
	public function getResults();

	/**
	 * Decodes logic input into statements and executes each statement removing un-matching results
	 *
	 * @throws Exception
	 *
	 * @return self
	 */
	public function execute();

}
