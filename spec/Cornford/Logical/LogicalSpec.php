<?php namespace spec\Cornford\Logical;

use PhpSpec\ObjectBehavior;
use Mockery;

class LogicalSpec extends ObjectBehavior {

	function let()
	{
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

	function it_should_decode_a_logic_string()
	{
		$input = [
			['name' => 'tom']
		];
		$logic = 'where("name").equals("tom")';
		$this->setInput($input);
		$this->setLogic($logic);
		$this->execute()->shouldReturn($this);
		$this->getDecodedLogicStatements()->shouldReturn(
			[[['method' => "equals", 'expected' => "tom", 'field' => "name"]]]
		);
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

}
