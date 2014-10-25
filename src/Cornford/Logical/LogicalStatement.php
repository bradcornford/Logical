<?php namespace Cornford\Logical;

use Cornford\Logical\Contracts\LogicalStatementInterface;

class LogicalStatement implements LogicalStatementInterface {

	/**
	 * Array of custom logical statements
	 *
	 * @var array
	 */
	protected $customStatements = [];

	/**
	* Define a custom logical statement
	*
	* @param string   $name     The statement name
	* @param callable $callback The callback method to execute
	*
	* @return void
	*/
	public function defineCustomStatement($name, callable $callback)
	{
		$this->customStatements[$name] = $callback;
	}

	/**
	* Checks that a custom logical statement exists
	*
	* @param string $name The statement name
	*
	* @return boolean
	*/
	public function customStatementExists($name)
	{
		if (!array_key_exists($name, $this->customStatements)) {
			return false;
		}

		return true;
	}

	/**
	* Returns all custom logical statements
	*
	* @return array
	*/
	public function getCustomStatements()
	{
		return $this->customStatements;
	}

	/**
	* Define a custom logical statement
	*
	* @param string                 $name     The statement name
	* @param string|integer|boolean $input    The input value
	* @param string|integer|boolean $expected The expected type
	*
	* @return boolean
	*/
	public function callCustomStatement($name, $input, $expected)
	{
		return call_user_func_array($this->customStatements[$name], [$input, $expected]);
	}

	/**
	* The input value equals the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function equals($input, $expected)
	{
		if ($input == $expected) {
			return true;
		}

		return false;
	}

	/**
	* The input value doesn't equal the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function notEquals($input, $expected)
	{
		return !$this->equals($input, $expected);
	}

	/**
	* The input value is of the expected type
	*
	* @param string $input The input value
	* @param string $expected The expected type
	*
	* @return boolean
	*/
	public function is($input, $expected)
	{
		$method = "is_$expected";

		if (function_exists($method))
		{
			if ($method($input)) {
				return true;
			}
		}

		return false;
	}

	/**
	* The input value doesn't equal the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function isNot($input, $expected)
	{
		return !$this->is($input, $expected);
	}

	/**
	* The input value contains the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function contains($input, $expected)
	{
		if (stristr($input, $expected)) {
			return true;
		}

		return false;
	}

	/**
	* The input value doesn't contain the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function notContains($input, $expected)
	{
		return !$this->contains($input, $expected);
	}

	/**
	* The input value is in the expected value array
	*
	* @param string $input The input value
	* @param array $expected The expected value array
	*
	* @return boolean
	*/
	public function in($input, $expected)
	{
		if (in_array($input, $expected)) {
			return true;
		}

		return false;
	}

	/**
	* The input value is not in the expected value array
	*
	* @param string $input The input value
	* @param array $expected The expected value array
	*
	* @return boolean
	*/
	public function notIn($input, $expected)
	{
		return !$this->in($input, $expected);
	}

	/**
	* The input value is between expected values
	*
	* @param string $input The input value
	* @param array $expected The expected values
	*
	* @return boolean
	*/
	public function between($input, $expected)
	{
		if (($expected[0] <= $input) && ($input <= $expected[1])) {
			return true;
		}

		return false;
	}

	/**
	* The input value is not between the expected values
	*
	* @param string $input The input value
	* @param array $expected The expected values
	*
	* @return boolean
	*/
	public function notBetween($input, $expected)
	{
		return !$this->between($input, $expected);
	}

	/**
	* The input value is null
	*
	* @param string $input The input value
	*
	* @return boolean
	*/
	public function null($input)
	{
		if ($input === null) {
			return true;
		}

		return false;
	}

	/**
	* The input value is not null
	*
	* @param string $input The input value
	*
	* @return boolean
	*/
	public function notNull($input)
	{
		return !$this->null($input);
	}

	/**
	* The input value is less than the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function lessThan($input, $expected)
	{
		if ($input < $expected) {
			return true;
		}

		return false;
	}

	/**
	* The input value isn't less than the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function notLessThan($input, $expected)
	{
		return !$this->lessThan($input, $expected);
	}

	/**
	* The input value is greater than the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function greaterThan($input, $expected)
	{
		if ($input > $expected) {
			return true;
		}

		return false;
	}

	/**
	* The input value isn't greater than the expected value
	*
	* @param string $input The input value
	* @param string $expected The expected value
	*
	* @return boolean
	*/
	public function notGreaterThan($input, $expected)
	{
		return !$this->lessThan($input, $expected);
	}

	/**
	 * The input value is less than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function lessThanOrEqual($input, $expected)
	{
		if ($input <= $expected) {
			return true;
		}

		return false;
	}

	/**
	 * The input value isn't less than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notLessThanOrEqual($input, $expected)
	{
		return !$this->lessThanOrEqual($input, $expected);
	}

	/**
	 * The input value is greater than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function greaterThanOrEqual($input, $expected)
	{
		if ($input >= $expected) {
			return true;
		}

		return false;
	}

	/**
	 * The input value isn't greater than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notGreaterThanOrEqual($input, $expected)
	{
		return !$this->greaterThanOrEqual($input, $expected);
	}

}