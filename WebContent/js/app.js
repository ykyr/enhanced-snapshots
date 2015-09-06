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
        if (!$rootScope.isConfigState()) throw "System is not in configuration state!";
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
            controller: function ($scope, $state, $rootScope) {
                $scope.isVolume = $state.current.name === 'app.volume.list';
                $rootScope.$on('$stateChangeSuccess',
                    function(event, toState, toParams, fromState, fromParams){
                        $scope.isVolume = $state.is('app.volume.list') || $state.is('app.users');
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
        .state('app.tasks', {
            url: "/tasks",
            templateUrl: "partials/tasks.html",
            controller: "TasksController"
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
            url: "/login",
            templateUrl: "partials/login.html",
            controller: "LoginController"
        })
        .state('registration', {
            url: "/registration",
            templateUrl: "partials/registration.html",
            controller: "RegistrationController"
        })
        .state('logout', {
            url: "/logout",
            controller: function ($state, Auth) {
                Auth.logOut();
                $state.go('login');
            }
        });

    $httpProvider.interceptors.push('Interceptor');
})
    .run(function ($rootScope, $state, Storage) {
        $rootScope.getUserName = function () {
            return (Storage.get("currentUser") || {}).email;
        };

        $rootScope.isConfigState = function () {
            return (Storage.get("currentUser") || {}).role === 'configurator';
        };

        $rootScope.$on('$stateChangeError', function (e) {
            e.preventDefault();
            $state.go('logout');
        });
    });

