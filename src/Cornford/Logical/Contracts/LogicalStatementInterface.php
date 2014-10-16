<?php namespace Cornford\Logical\Contracts;

interface LogicalStatementInterface {

	/**
	 * Define a custom logical statement
	 *
	 * @param string   $name     The statement name
	 * @param callable $callback The callback method to execute
	 *
	 * @return boolean
	 */
	public function defineCustomStatement($name, callable $callback);

	/**
	 * Checks that a custom logical statement exists
	 *
	 * @param string $name The statement name
	 *
	 * @return boolean
	 */
	public function customStatementExists($name);

	/**
	 * Returns all custom logical statements
	 *
	 * @return array
	 */
	public function getCustomStatements();

	/**
	 * Define a custom logical statement
	 *
	 * @param string $name     The statement name
	 * @param string $input    The input value
	 * @param string $expected The expected type
	 *
	 * @return boolean
	 */
	public function callCustomStatement($name, $input, $expected);

	/**
	 * The input value equals the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function equals($input, $expected);

	/**
	 * The input value doesn't equal the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notEquals($input, $expected);

	/**
	 * The input value is of the expected type
	 *
	 * @param string $input The input value
	 * @param string $expected The expected type
	 *
	 * @return boolean
	 */
	public function is($input, $expected);

	/**
	 * The input value doesn't equal the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function isNot($input, $expected);

	/**
	 * The input value contains the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function contains($input, $expected);

	/**
	 * The input value doesn't contain the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notContains($input, $expected);

	/**
	 * The input value is in the expected value array
	 *
	 * @param string $input The input value
	 * @param array $expected The expected value array
	 *
	 * @return boolean
	 */
	public function in($input, $expected);

	/**
	 * The input value is not in the expected value array
	 *
	 * @param string $input The input value
	 * @param array $expected The expected value array
	 *
	 * @return boolean
	 */
	public function notIn($input, $expected);

	/**
	 * The input value is between expected values
	 *
	 * @param string $input The input value
	 * @param array $expected The expected values
	 *
	 * @return boolean
	 */
	public function between($input, $expected);

	/**
	 * The input value is not between the expected values
	 *
	 * @param string $input The input value
	 * @param array $expected The expected values
	 *
	 * @return boolean
	 */
	public function notBetween($input, $expected);

	/**
	 * The input value is null
	 *
	 * @param string $input The input value
	 *
	 * @return boolean
	 */
	public function null($input);

	/**
	 * The input value is not null
	 *
	 * @param string $input The input value
	 *
	 * @return boolean
	 */
	public function notNull($input);

	/**
	 * The input value is less than the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function lessThan($input, $expected);

	/**
	 * The input value isn't less than the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notLessThan($input, $expected);

	/**
	 * The input value is greater than the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function greaterThan($input, $expected);

	/**
	 * The input value isn't greater than the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notGreaterThan($input, $expected);

	/**
	 * The input value is less than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function lessThanOrEqual($input, $expected);

	/**
	 * The input value isn't less than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notLessThanOrEqual($input, $expected);

	/**
	 * The input value is greater than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function greaterThanOrEqual($input, $expected);

	/**
	 * The input value isn't greater than or equal to the expected value
	 *
	 * @param string $input The input value
	 * @param string $expected The expected value
	 *
	 * @return boolean
	 */
	public function notGreaterThanOrEqual($input, $expected);

}
