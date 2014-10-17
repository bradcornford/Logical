<?php namespace spec\Cornford\Logical;

use PhpSpec\ObjectBehavior;
use Mockery;

class LogicalSpec extends ObjectBehavior {

	protected $visiblePropertiesClass;
	protected $getterPropertiesClass;

	function let()
	{
		$visiblePropertiesClass = Mockery::mock('visibleProperties');
		$this->visiblePropertiesClass = $visiblePropertiesClass;

		$getterPropertiesClass = Mockery::mock('getterProperties');
		$getterPropertiesClass->name = "tom";
		$getterPropertiesClass->shouldReceive('getName')->andReturn($getterPropertiesClass->name);
		$this->getterPropertiesClass = $getterPropertiesClass;

		$logicalStatement = Mockery::mock('Cornford\Logical\LogicalStatement');
		$logicalStatement->shouldReceive('equals')->andReturn(true);
		$logicalStatement->shouldReceive('customStatementExists')->andReturn(false);

		$input = [];
		$logic = null;

		$this->beConstructedWith($input, $logic, $logicalStatement);
	}

	function it_is_initializable()
	{
		$this->shouldHaveType('Cornford\Logical\Contracts\LogicalInterface');
	}

	function it_should_allow_a_logical_statement_instance_to_be_set()
	{
		$input = Mockery::mock('Cornford\Logical\LogicalStatement');
		$this->setLogicalStatementInstance($input);
		$this->getLogicalStatementInstance()->shouldReturn($input);
	}

	function it_should_allow_input_to_be_set()
	{
		$input = ['key' => 'value'];
		$this->setInput($input);
		$this->getInput()->shouldReturn($input);
	}

	function it_should_allow_logic_to_be_set()
	{
		$input = 'logic string';
		$this->setLogic($input);
		$this->getLogic()->shouldReturn($input);
	}

	function it_should_throw_an_exception_when_unable_to_decode_logic()
	{
		$input = [
			['name' => 'tom']
		];
		$logic = 'thisisntlogic';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->shouldThrow('\Cornford\Logical\Exceptions\LogicalDecodingException')->during('execute');
	}

	function it_should_throw_an_exception_when_unable_to_find_a_field_value()
	{
		$input = [
			['notname' => 'tom']
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->shouldThrow('\Cornford\Logical\Exceptions\LogicalFieldValueException')->during('execute');
	}

	function it_should_throw_an_exception_when_unable_to_execute_logic_statement()
	{
		$input = [
			['name' => 'tom']
		];
		$logic = 'where("name").something("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->shouldThrow('\Cornford\Logical\Exceptions\LogicalExecutionException')->during('execute');
	}

	function it_should_execute_the_decoded_logic_statements_on_an_array()
	{
		$input = [
			['name' => 'tom']
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->execute()->shouldReturn($this);
		$this->getResults()->shouldReturn($input);
	}

	function it_should_execute_the_decoded_logic_statements_removing_invalid_results_on_an_array()
	{
		$input = [
			['name' => 'tom'],
			['name' => 'paul'],
		];
		$output = [
			['name' => 'tom']
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->execute()->shouldReturn($this);
		$this->getResults()->shouldReturn($input);
	}

	function it_should_execute_the_decoded_logic_statements_removing_invalid_results_on_a_class_with_visible_properties()
	{
		$input1 = $this->visiblePropertiesClass;
		$input1->name = "tom";

		$input2 = $this->visiblePropertiesClass;
		$input2->name = "paul";

		$input = [
			$input1,
			$input2
		];
		$output = [
			$input1,
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->execute()->shouldReturn($this);
		$this->getResults()->shouldReturn($input);
	}

	function it_should_execute_the_decoded_logic_statements_removing_invalid_results_on_a_class_with_getter_properties()
	{
		$input1 = $this->getterPropertiesClass;
		$input1->name = "tom";

		$input2 = $this->getterPropertiesClass;
		$input2->name = "paul";

		$input = [
			$input1,
			$input2
		];
		$output = [
			$input1,
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->execute()->shouldReturn($this);
		$this->getResults()->shouldReturn($input);
	}

}
