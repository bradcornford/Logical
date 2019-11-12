<?php namespace spec\Cornford\Logical;

use PhpSpec\ObjectBehavior;
use Mockery;

class LogicalFactorySpec extends ObjectBehavior
{
    public function it_is_initializable()
    {
        $this->shouldHaveType('Cornford\Logical\LogicalFactory');
    }

    public function it_can_build_a_logical_object()
    {
        $this->build()->shouldHaveType('Cornford\Logical\Contracts\LogicalInterface');
    }

    public function it_can_build_a_logical_statement_object()
    {
        $this->buildLogicalStatement()->shouldHaveType('Cornford\Logical\Contracts\LogicalStatementInterface');
    }

    public function it_can_build_a_logical_object_with_a_logical_statement()
    {
        $this->buildLogical([], '', $this->buildLogicalStatement())->shouldHaveType('Cornford\Logical\Contracts\LogicalInterface');
    }
}
