<?php namespace spec\Cornford\Logical;

use PhpSpec\ObjectBehavior;
use Mockery;

class LogicalStatementSpec extends ObjectBehavior
{
    public function it_is_initializable()
    {
        $this->shouldHaveType('Cornford\Logical\Contracts\LogicalStatementInterface');
    }

    public function it_can_define_a_custom_statement()
    {
        $name = 'test';
        $method = function () {
            return true;
        };
        $this->defineCustomStatement($name, $method);
        $this->customStatementExists($name)->shouldReturn(true);
        $this->getCustomStatements()->shouldReturn([$name => $method]);
    }

    public function it_can_execute_a_custom_defined_statement()
    {
        $name = 'test';
        $input = 'name';
        $expected = 'name';
        $method = function ($input, $expected) {
            return true;
        };
        $this->defineCustomStatement($name, $method);
        $this->callCustomStatement($name, $input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_an_equals_statement()
    {
        $input = 'name';
        $expected = 'name';
        $this->equals($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_equals_statement()
    {
        $input = 'name';
        $expected = 'notname';
        $this->notEquals($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_an_is_length_statement()
    {
        $input = 'name';
        $expected = 4;
        $this->isLength($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_is_not_length_statement()
    {
        $input = 'name';
        $expected = 5;
        $this->isNotLength($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_an_is_statement()
    {
        $input = 'name';
        $expected = 'string';
        $this->is($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_an_is_not_statement()
    {
        $input = 'name';
        $expected = 'boolean';
        $this->isNot($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_contains_statement()
    {
        $input = 'name';
        $expected = 'a';
        $this->contains($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_contains_statement()
    {
        $input = 'name';
        $expected = 'b';
        $this->notContains($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_contained_in_statement()
    {
        $input = 'name';
        $expected = ['a', 'b', 'c'];
        $this->containedIn($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_contained_in_statement()
    {
        $input = 'name';
        $expected = ['b', 'c', 'd'];
        $this->notContainedIn($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_an_in_statement()
    {
        $input = 2;
        $expected = [1, 2, 3];
        $this->in($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_in_statement()
    {
        $input = 4;
        $expected = [1, 2, 3];
        $this->notIn($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_between_statement()
    {
        $input = 2;
        $expected = [1, 3];
        $this->between($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_between_statement()
    {
        $input = 4;
        $expected = [1, 3];
        $this->notBetween($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_null_statement()
    {
        $input = null;
        $this->null($input)->shouldReturn(true);
    }

    public function it_can_execute_a_not_null_statement()
    {
        $input = 'notnull';
        $this->notNull($input)->shouldReturn(true);
    }

    public function it_can_execute_a_less_than_statement()
    {
        $input = 1;
        $expected = 2;
        $this->lessThan($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_less_than_statement()
    {
        $input = 2;
        $expected = 2;
        $this->notLessThan($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_greater_than_statement()
    {
        $input = 2;
        $expected = 1;
        $this->greaterThan($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_greater_than_statement()
    {
        $input = 1;
        $expected = 1;
        $this->notGreaterThan($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_less_than_or_equal_statement()
    {
        $input = 1;
        $expected = 1;
        $this->lessThanOrEqual($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_less_than_or_equal_statement()
    {
        $input = 3;
        $expected = 2;
        $this->notLessThanOrEqual($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_greater_than_or_equal_statement()
    {
        $input = 1;
        $expected = 1;
        $this->greaterThanOrEqual($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_greater_than_or_equal_statement()
    {
        $input = 1;
        $expected = 2;
        $this->notGreaterThanOrEqual($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_starts_with_statement()
    {
        $input = 'this is a string';
        $expected = 'this';
        $this->startsWith($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_starts_with_statement()
    {
        $input = 'also this is a string';
        $expected = 'this';
        $this->notStartsWith($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_ends_with_statement()
    {
        $input = 'this is a string';
        $expected = 'string';
        $this->endsWith($input, $expected)->shouldReturn(true);
    }

    public function it_can_execute_a_not_ends_with_statement()
    {
        $input = 'this is a string also';
        $expected = 'string';
        $this->notEndsWith($input, $expected)->shouldReturn(true);
    }
}
