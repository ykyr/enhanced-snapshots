var app = angular.module('web', ['ui.router', 'angularAwesomeSlider', 'ui.bootstrap', 'smart-table', 'ngTagsInput']);

app.constant('BASE_URL', './');

// Settings for table paging
app.constant('ITEMS_BY_PAGE', 25);
app.constant('DISPLAY_PAGES', 5);

app.config(function ($stateProvider, $urlRouterProvider, $httpProvider) {
    $urlRouterProvider.otherwise("/app/volumes");

    var authenticated = ['$rootScope', function ($rootScope) {
        if (angular.isUndefined($rootScope.getUserName())) throw "User not authorized!";
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
        .state('aws', {
            url: "/aws",
            templateUrl: "partials/aws.html",
            controller: "AwsController",
            resolve: {
                authenticated: authenticated
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
            resolve: {
                logout: logout
            }
        });

    $httpProvider.interceptors.push('Interceptor');
})
    .run(function ($rootScope, $state, Storage) {
        $rootScope.getUserName = function () {
            return (Storage.get("currentUser") || {}).email;
        };
        $rootScope.$on('$stateChangeError', function (e) {
            e.preventDefault();
            $state.go('login');
        });
    });

