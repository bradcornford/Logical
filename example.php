<?php

require_once __DIR__ . "/vendor/autoload.php";

use Cornford\Logical\LogicalFactory;

class Person
{
    /**
     * @var integer
     */
    protected $age;

    /**
     * @var string
     */
    protected $email;
    protected $forename;
    protected $surname;
    protected $title;

    /**
     * @param array $array
     */
    public function __construct(array $array = [])
    {
        if (isset($array['title'])) {
            $this->title = $array['title'];
        }
        if (isset($array['forename'])) {
            $this->forename = $array['forename'];
        }
        if (isset($array['surname'])) {
            $this->surname = $array['surname'];
        }
        if (isset($array['email'])) {
            $this->email = $array['email'];
        }
        if (isset($array['age'])) {
            $this->age = $array['age'];
        }
    }

    /**
     * @param string         $name
     * @param string|integer $value
     */
    public function __set($name, $value)
    {
        if (property_exists($this, $name)) {
            $this->$name = $value;
        }
    }

    /**
     * @param string         $name
     *
     * @return string|integer|void
     */
    public function __get($name)
    {
        if (property_exists($this, $name)) {
            return $this->$name;
        }
    }
}

$people = [];
$people[] = new Person(['title' => 'Mr', 'forename' => 'Tom', 'surname' => 'Guy', 'age' => 17, 'email' => 'test@guy.com']);
$people[] = new Person(['title' => 'Mrs', 'forename' => 'Holly', 'surname' => 'Berry', 'age' => 28, 'email' => 'test@berry.co.uk']);
$people[] = new Person(['title' => 'Miss', 'forename' => 'Hannah', 'surname' => 'Pots', 'age' => 22, 'email' => 'test@pots.com']);
$people[] = new Person(['title' => 'Mr', 'forename' => 'David', 'surname' => 'Summers', 'age' => 24, 'email' => 'test@summers.gov.uk']);
$people[] = new Person(['title' => 'Dr', 'forename' => 'Paul', 'surname' => 'Sky', 'age' => 18, 'email' => 'test@sky.co.uk']);
$people[] = new Person(['title' => 'Mr', 'forename' => 'Steve', 'surname' => 'Smith', 'age' => 27, 'email' => 'steve@smith.org']);
$people[] = new Person(['title' => 'Mr', 'forename' => 'Philip', 'surname' => 'Comms', 'age' => 29, 'email' => 'philip.comms@test.co']);

$logicStatement = 'where("forename").equals("Tom").OR.where("age").between(18, 21).OR.where("email").contains(".co.uk").AND.where("forename").equals("Holly")';
$logicalFactory = new LogicalFactory();
$logical = $logicalFactory->build($people, $logicStatement);
$logical->execute();
$results = $logical->getResults();

var_dump($results);
