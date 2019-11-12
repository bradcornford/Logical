<?php namespace Cornford\Logical\Providers;

use Cornford\Logical\LogicalFactory;
use Illuminate\Support\ServiceProvider;

class LogicalServiceProvider extends ServiceProvider
{

    /**
     * Indicates if loading of the provider is deferred.
     *
     * @var bool
     */
    protected $defer = true;

    /**
     * Bootstrap the application events.
     *
     * @return void
     */
    public function boot()
    {
//        $this->package('cornford/logical');
    }

    /**
     * Register the service provider.
     *
     * @return void
     */
    public function register()
    {
        $this->app->singleton(
            'logical',
            function ($app) {
                return (new LogicalFactory)->build();
            }
        );
    }

    /**
     * Get the services provided by the provider.
     *
     * @return string[]
     */
    public function provides()
    {
        return ['logical'];
    }
}
