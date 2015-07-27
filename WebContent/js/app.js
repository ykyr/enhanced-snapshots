var app = angular.module('web', ['ui.router', 'ui.bootstrap']);

app.constant('BASE_URL', './');
//app.constant('BASE_URL', 'http://localhost:8080/snapdirector01/');

app.config(function ($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise("/app/volumes");

    var authenticated = ['$q', 'Auth', function ($q, Auth) {
        var deferred = $q.defer();
        if (Auth.isLoggedIn()) {
            deferred.resolve(true);
        } else {
            deferred.reject('Not logged in');
        }
        return deferred.promise;
    }];

    var hasUsers = ['$state', 'Auth', function ($state, Auth) {
        Auth.hasUsers().then(function (result) {
            if (!result) {
                $state.go('registration');
            }else{
                return false;
            }
        });
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
            url: "/schedule/:volumeID",
            templateUrl: "partials/schedule.html",
            controller: 'ScheduleController'
        })

        .state('app.volume.history', {
            url: "/history/:volumeID",
            templateUrl: "partials/history.html",
            controller: 'HistoryController'
        })
        .state('app.tasks', {
            url: "/tasks",
            templateUrl: "partials/tasks.html",
            controller: "TasksController"
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
            controller: "LoginController",
            resolve: {
                hasUsers: hasUsers
            }
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
})
    .run(function ($rootScope, $state) {
        $rootScope.$on('$stateChangeError', function (e) {
            e.preventDefault();
            $state.go('login');
        });
    });
;