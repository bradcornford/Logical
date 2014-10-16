<?php namespace spec\Cornford\Logical;

use PhpSpec\ObjectBehavior;
use Mockery;

class LogicalFactorySpec extends ObjectBehavior {

	function it_is_initializable()
	{
		$this->shouldHaveType('Cornford\Logical\LogicalFactory');
	}

	function it_can_build_a_logical_object()
	{
		$this->build()->shouldHaveType('Cornford\Logical\Contracts\LogicalInterface');
	}

	function it_can_build_a_logical_statement_object()
	{
		$this->buildLogicalStatement()->shouldHaveType('Cornford\Logical\Contracts\LogicalStatementInterface');
	}

	function it_can_build_a_logical_object_with_a_logical_statement()
	{
		$this->buildLogical([], '', $this->buildLogicalStatement())->shouldHaveType('Cornford\Logical\Contracts\LogicalInterface');
	}

}
