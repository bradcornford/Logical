<?php namespace Cornford\Logical;

use Cornford\Logical\Contracts\LogicalStatementInterface;
use Symfony\Component\PropertyAccess\PropertyAccess;
use Symfony\Component\PropertyAccess\PropertyAccessor;

abstract class LogicalAbstract {

	/**
	 * Logical statement instance
	 *
	 * @var \Cornford\Logical\Contracts\LogicalStatementInterface
	 */
	protected static $logicalStatementInstance;

	/**
	 * Property accessor instance
	 *
	 * @var \Symfony\Component\PropertyAccess\PropertyAccessor
	 */
	protected static $propertyAccessorInstance;

	/**
	 * The input array
	 *
	 * @var array
	 */
	protected $input = [];

	/**
	 * The logic string
	 *
	 * @var string
	 */
	protected $logic;

	/**
	 * The decoded logic statements
	 *
	 * @var array
	 */
	protected $decodedLogicStatements = [];

	/**
	 * The output results
	 *
	 * @var array
	 */
	protected $results = [];

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
	) {
		self::$logicalStatementInstance = $logicalStatement;
		self::$propertyAccessorInstance = PropertyAccess::createPropertyAccessor();
		$this->input = $input;
		$this->logic = $logic;
	}

	/**
	 * Set the logical statement instance
	 *
	 * @param LogicalStatementInterface $value
	 *
	 * @return void
	 */
	public function setLogicalStatementInstance(LogicalStatementInterface $value)
	{
		self::$logicalStatementInstance = $value;
	}

	/**
	 * Get the logic statement instance
	 *
	 * @return LogicalStatementInterface
	 */
	public function getLogicalStatementInstance()
	{
		return self::$logicalStatementInstance;
	}

	/**
	 * Get the property accessor instance
	 *
	 * @return PropertyAccessor
	 */
	protected function getPropertyAccessorInstance()
	{
		return self::$propertyAccessorInstance;
	}

	/**
	 * Set the input value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	public function setInput(array $value = [])
	{
		$this->input = $value;
	}

	/**
	 * Get the input value
	 *
	 * @return array
	 */
	public function getInput()
	{
		return $this->input;
	}

	/**
	 * Set the logic value
	 *
	 * @param string $value
	 *
	 * @return void
	 */
	public function setLogic($value)
	{
		$this->logic = $value;
	}

	/**
	 * Get the logic value
	 *
	 * @return string
	 */
	public function getLogic()
	{
		return $this->logic;
	}

	/**
	 * Set a decoded logic statements value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	protected function setDecodedLogicStatements($value)
	{
		$this->decodedLogicStatements = $value;
	}

	/**
	 * Set a decoded logic statement value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	protected function setDecodedLogicStatement($value)
	{
		if (!array_key_exists(key($value), $this->decodedLogicStatements)) {
			$this->decodedLogicStatements[key($value)] = [];
		}

		$this->decodedLogicStatements[key($value)] = array_merge($this->decodedLogicStatements[key($value)], $value);
	}

	/**
	 * Get the decoded logic statements value
	 *
	 * @return array
	 */
	protected function getDecodedLogicStatements()
	{
		return $this->decodedLogicStatements;
	}

	/**
	 * Set the results value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	protected function setResults($value)
	{
		$this->results = $value;
	}

	/**
	 * Merge the results value with an input results value
	 *
	 * @param array $value
	 *
	 * @return void
	 */
	protected function mergeResults($value)
	{
		foreach ($value as $key => $result) {
			if (!array_key_exists($key, $this->results)) {
				$this->results[$key] = $result;
			}
		}
	}

	/**
	 * Get the results value
	 *
	 * @return array
	 */
	public function getResults()
	{
		return $this->results;
	}

}