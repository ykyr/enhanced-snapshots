var app = angular.module('web', ['ui.router', 'angularAwesomeSlider', 'ui.bootstrap', 'smart-table', 'ngTagsInput', 'toastr']);

app.constant('BASE_URL', './');

// Settings for table paging
app.constant('ITEMS_BY_PAGE', 25);
app.constant('DISPLAY_PAGES', 7);

app.config(function ($stateProvider, $urlRouterProvider, $httpProvider) {
    $urlRouterProvider.otherwise("/app/volumes");

    var authenticated = ['$rootScope', function ($rootScope) {
        if (angular.isUndefined($rootScope.getUserName())) throw "User not authorized!";
        return true;
    }];

    var isConfig = ['$rootScope', function ($rootScope) {
        if (!$rootScope.isConfigState())  throw "System is not in configuration state!";
        return true;
    }];

    var logout = ['$q', 'Auth', function ($q, Auth) {
        Auth.logOut();
        return $q.reject('Logged out');
    }];

    $stateProvider
        .state('app', {
            abstract: true,
            url: "/app",
            templateUrl: "partials/app.html",
            resolve: {
                authenticated: authenticated
            },
            controller: function ($scope, $rootScope, Storage, toastr) {
                $rootScope.$on('$stateChangeSuccess',
                    function(event, toState, toParams, fromState, fromParams){
                        var notification = Storage.get("notification");
                        if (notification) {
                            toastr.info(notification);
                            Storage.remove("notification");
                        }
                    });
            }
        })
        .state('app.volume', {
            abstract: true,
            template: "<ui-view></ui-view>",
            url: ""
        })
        .state('app.volume.list', {
            url: "/volumes",
            templateUrl: "partials/volumes.html",
            controller: 'VolumesController'
        })
        .state('app.volume.schedule', {
            url: "/schedule/:volumeId",
            templateUrl: "partials/schedule.html",
            controller: 'ScheduleController'
        })

        .state('app.volume.history', {
            url: "/history/:volumeId",
            templateUrl: "partials/history.html",
            controller: 'HistoryController'
        })
        .state('app.volume.tasks', {
            url: "/tasks/:volumeId",
            templateUrl: "partials/tasks.html",
            controller: "TasksController"
        })
        .state('app.tasks', {
            url: "/tasks",
            templateUrl: "partials/tasks.html",
            controller: "TasksController"
        })
        .state('app.settings', {
            url: "/settings",
            templateUrl: "partials/settings.html",
            controller: "SettingsController"
        })
        .state('app.users', {
            url: "/users",
            templateUrl: "partials/users.html",
            controller: "UserController"
        })
        .state('config', {
            url: "/config",
            templateUrl: "partials/config.html",
            controller: "ConfigController",
            resolve: {
                isConfig: isConfig
            }
        })
        .state('login', {
            url: "/login?err",
            templateUrl: "partials/login.html",
            controller: "LoginController"
        })
        .state('registration', {
            url: "/registration",
            templateUrl: "partials/registration.html",
            controller: "RegistrationController"
        })
        /*.state('logout', {
            url: "/logout",
            controller: function ($state, Auth) {
                Auth.logOut();
                $state.go('login');
            }
        })*/;

    $httpProvider.interceptors.push('Interceptor');
})
    .run(function ($rootScope, $state, $modal, Storage, System) {
        $rootScope.getUserName = function () {
            return (Storage.get("currentUser") || {}).email;
        };

        $rootScope.isConfigState = function () {
            return (Storage.get("currentUser") || {}).role === 'configurator';
        };

        $rootScope.isLoading = false;

        $rootScope.$on('$stateChangeError', function (e) {
            e.preventDefault();
            $state.go('login');
        });
    });

