<?php namespace Cornford\Logical;

use Cornford\Logical\Contracts\LogicalInterface;
use Cornford\Logical\Exceptions\LogicalDecodingException;
use Cornford\Logical\Exceptions\LogicalExecutionException;
use Cornford\Logical\Exceptions\LogicalFieldValueException;
use Exception;

class Logical extends LogicalAbstract implements LogicalInterface {

	/**
	 * The temporary logic method
	 *
	 * @var string
	 */
	protected $tempLogic;

	/**
	 * The temporary logic method
	 *
	 * @var string
	 */
	protected $tempMethod;

	/**
	 * The temporary logic expected
	 *
	 * @var string|array
	 */
	protected $tempExpected;

	/**
	 * The temporary logic field
	 *
	 * @var string
	 */
	protected $tempField;

	/**
	 * The temporary results
	 *
	 * @var array
	 */
	protected $tempResults;

	/**
	 * Decode the logic string into an array of logical statements
	 *
	 * @return boolean
	 */
	protected function decodeLogicStatements()
	{
		$orStatements = preg_split("/.OR./", $this->getLogic());

		foreach ($orStatements as $orKey => $orStatement) {
			$andStatements = preg_split("/.AND./", $orStatement);

			foreach ($andStatements as $andStatement) {
				$items = preg_split("/\./", $andStatement);
				$decodedStatement = [];

				foreach ($items as $item) {
					$matches = null;

					if (stristr($item, 'where')) {
						if (!preg_match("/\((.*)\)/", $item, $matches)) {
							return false;
						}

						$this->tempField = str_replace('"', '', $matches[1]);

						continue;
					}

					if (!preg_match("/(.*)\((.*)\)/", $item, $matches)) {
						return false;
					} else {
						$this->tempMethod = str_replace('"', '', $matches[1]);
						$this->tempExpected = str_replace('"', '', $matches[2]);
					}

					if (stristr($this->tempExpected, ', ')) {
						$this->tempExpected = explode(', ', $this->tempExpected);
					}

					$decodedStatement[$orKey]['method'] = $this->tempMethod;
					$decodedStatement[$orKey]['expected'] = $this->tempExpected;
					$decodedStatement[$orKey]['field'] = $this->tempField;
				}

				$this->setDecodedLogicStatement($decodedStatement);
			}
		}

		return true;
	}

	/**
	 * Execute a logic statement method on an input value against an expected value
	 *
	 * @param string                 $method
	 * @param string|integer|boolean $input
	 * @param string|integer|boolean $expected
	 *
	 * @throws LogicalExecutionException
	 *
	 * @return boolean
	 */
	protected function executeLogicStatement($method, $input, $expected = null)
	{
		$logicalStatementInstance = $this->getLogicalStatementInstance();
		$result = null;

		if (method_exists($logicalStatementInstance, $method)) {
			$result = $logicalStatementInstance->$method($input, $expected);
		}

		if ($logicalStatementInstance->customStatementExists($method)) {
			$result = $logicalStatementInstance->callCustomStatement($method, $input, $expected);
		}

		if (is_null($result)) {
			throw new LogicalExecutionException("Unable to execute logic statement method: {$method}().");
		}

		return $result;
	}

	/**
	 * Decodes logic input into statements and executes each statement removing un-matching results
	 *
	 * @throws LogicalDecodingException
	 * @throws LogicalFieldValueException
	 *
	 * @return self
	 */
	public function execute()
	{
		if (!$this->decodeLogicStatements()) {
			throw new LogicalDecodingException('Unable to decode logic input.');
		}

		foreach ($this->getDecodedLogicStatements() as $orStatement) {
			$this->tempResults = $this->getInput();

			foreach ($this->getInput() as $key => $input) {
				foreach ($orStatement as $andStatement) {
					switch (gettype($input)) {
						case 'object':
							break;
						case 'array':
						default:
							$andStatement['field'] = '[' . $andStatement['field'] . ']';
					}

					try {
						$value = $this->getPropertyAccessorInstance()->getValue($input, $andStatement['field']);
					} catch (Exception $exception) {
						throw new LogicalFieldValueException('Unable to locate logical statement field value.');
					}

					if (!$this->executeLogicStatement($andStatement['method'], $value, $andStatement['expected'])) {
						unset($this->tempResults[$key]);
					}
				}
			}

			$this->mergeResults($this->tempResults);
		}

		return $this;
	}

}