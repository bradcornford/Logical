# An easy way execute logical statements over a none persisted results set in Laravel.

[![Latest Stable Version](https://poser.pugx.org/cornford/logical/version.png)](https://packagist.org/packages/cornford/logical)
[![Total Downloads](https://poser.pugx.org/cornford/logical/d/total.png)](https://packagist.org/packages/cornford/logical)
[![Build Status](https://travis-ci.org/bradcornford/Logical.svg?branch=master)](https://travis-ci.org/bradcornford/Logical)
[![Scrutinizer Code Quality](https://scrutinizer-ci.com/g/bradcornford/Logical/badges/quality-score.png?b=master)](https://scrutinizer-ci.com/g/bradcornford/Logical/?branch=master)

### For Laravel 4.x, check [version 1.2.0](https://github.com/bradcornford/Logical/tree/v1.2.0)

Think of Logical as an easy way to execute logical statements over a none persisted results set with Laravel, providing a way to reduce a results set against a logical sentence. These include:

- `Logical::setLogicalStatementInstance`
- `Logical::getLogicalStatementInstance`
- `Logical::setInput`
- `Logical::getInput`
- `Logical::setLogic`
- `Logical::getLogic`
- `Logical::execute`
- `Logical::getResults`
- `Logical::reset`

## Installation

Begin by installing this package through Composer. Edit your project's `composer.json` file to require `cornford/logical`.

	"require": {
		"cornford/logical": "2.*"
	}

Next, update Composer from the Terminal:

	composer update

Once this operation completes, the next step is to add the service provider. Open `app/config/app.php`, and add a new item to the providers array.

	'Cornford\Logical\Providers\LogicalServiceProvider',

The final step is to introduce the facade. Open `app/config/app.php`, and add a new item to the aliases array.

	'Logical'         => 'Cornford\Logical\Facades\Logical',

That's it! You're all set to go.

## Usage

It's really as simple as using the Logical class in any Controller / Model / File you see fit with:

`Logical::`

This will give you access to

- [Set Logical Statement Instance](#set-logical-statement-instance)
- [Get Logical Statement Instance](#get-logical-statement-instance)
- [Set Input](#set-input)
- [Get Input](#get-input)
- [Set Logic](#set-logic)
- [Get Logic](#get-logic)
- [Execute](#execute)
- [Get Results](#get-results)
- [Reset](#reset)

### Set Logical Statement Instance

The `setLogicalStatementInstance` method allows a custom Logical Statement Instance to be passed to after construction.

	Logical::setLogicalStatementInstance(new LogicalStatement);
	Logical::setLogicalStatementInstance(new MyLogicalStatement);

### Get Logical Statement Instance

The `getLogicalStatementInstance` method returns the set Logical Statement Instance.

	Logical::getLogicalStatementInstance();
	Logical::getLogicalStatementInstance()->defineCustomStatement('test', function ($input, $expected) { return true; });

### Set Input

The `setInput` method sets a input array set.

	Logical::setInput([['name' => 'tom'], ['name' => 'jerry']]);

### Get Input

The `getInput` method returns the set input array set.

	Logical::getInput();

### Set Logic

The `setLogic` method allows a logic statement string to be set. The available logical methods are within the Logical Statement class.

	Logical::setLogic('where("name").equals("tom")');

### Get Logic

The `getLogic` method returns the set logic statement string.

	Logical::getLogic();

### Execute

The `execute` method decodes the logic string into callable methods and then executes those methods against the input array.

	Logical::execute();

### Get Results

The `getResults` method returns the items matching the logic statement sting from the input array set.

	Logical::getResults();

### Reset

The `reset` method resets input, logic and results.

	Logical::reset();

## Building Logic Statement Strings

Logic statement strings are built by defining as follows:

- A field that should be conditioned against `where("field")`.
- A statement method that can executed `where("field").equals("tom")`.
- A conditions and methods are separated with a `.`.
- Multiple conditions and methods can be attached using `AND` or `OR` such as `where("field").equals("tom").OR.where("field").equals("jerry")`.

### Logic Statement String Methods

Below is a list of available logic statement string methods:

- [Equals](#equals)
- [Is Length](#is-length)
- [Is](#is)
- [Contains](#contains)
- [Contained In](#contained-in)
- [In](#in)
- [Between](#between)
- [Null](#null)
- [Less Than](#less-than)
- [Greater Than](#greater-than)
- [Less Than Or Equal](#less-than-or-equal)
- [Greater Than Or Equal](#greater-than-or-equal)
- [Starts With](#starts-with)
- [Ends With](#ends-with)

### Equals

The `equals` method returns true if the input value equals the expected value.

	equals(1)

The `notEquals` method returns true if the input value doesn't equals the expected value.

	notEquals(1)

### Is Length

The `isLength` method returns true if the input value equals the expected length value.

	isLength(1)

The `isNotLength` method returns true if the input value doesn't equals the expected length value.

	isNotLength(1)

### Is

The `is` method returns true if the input value is the same type the expected value.

	is("boolean")

The `isNot` method returns true if the input value isn't the same type the expected value.

	isNot("boolean")

### Contains

The `contains` method returns true if the input value contains the expected value.

	contains("o")

The `notContains` method returns true if the input value doesn't contain the expected value.

	notContains("o")

### Contained In

The `containedIn` method returns true if the input value contains an item of the expected values.

	containedIn("a", "b", "c")

The `notContainedIn` method returns true if the input value doesn't contain an item of the expected values.

	notContainedIn("a", "b", "c")

### In

The `in` method returns true if the input value is an item of the expected values.

	in("a", "b", "c")

The `notIn` method returns true if the input value is not an item of the expected values.

	notIn("a", "b", "c")

### Between

The `between` method returns true if the input value is between the expected values.

	between(1, 10)

The `notBetween` method returns true if the input value is not between the expected values.

	notBetween(1, 10)

### Null

The `null` method returns true if the input value is null.

	null()

The `notNull` method returns true if the input value is not null.

	notNull()

### Less Than

The `lessThan` method returns true if the input value is less than the expected value.

	lessThan(10)

The `notLessThan` method returns true if the input value is not less than the expected value.

	notLessThan(10)

### Greater Than

The `greaterThan` method returns true if the input value is greater than the expected value.

	greaterThan(10)

The `notGreaterThan` method returns true if the input value is not greater than the expected value.

	notGreaterThan(10)

### Less Than Or Equal

The `lessThanOrEqual` method returns true if the input value is less than or equal to the expected value.

	lessThanOrEqual(10)

The `notLessThanOrEqual` method returns true if the input value is not less than or not equal to the expected value.

	notLessThanOrEqual(10)

### Greater Than Or Equal

The `greaterThanOrEqual` method returns true if the input value is greater than or equal to the expected value.

	greaterThanOrEqual(10)

The `notGreaterThanOrEqual` method returns true if the input value is not greater than or not equal to the expected value.

	notGreaterThanOrEqual(10)

### Starts With

The `startsWith` method returns true if the input value starts with the expected value.

	startsWith('hello')

The `notStartsWith` method returns true if the input value doesn't start with the expected value.

	notStartsWith('hello')

### Ends With

The `endsWith` method returns true if the input value ends with the expected value.

	endsWith('goodbye')

The `notEndsWith` method returns true if the input value doesn't end with the expected value.

	notEndsWith('goodbye')

### License

Logical is open-sourced software licensed under the [MIT license](http://opensource.org/licenses/MIT)