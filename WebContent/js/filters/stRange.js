'use strict';

angular.module('web')
.filter('stRangeFilter', ['$filter', function ($filter) {
    var filterFilter = $filter('filter');
    var standardComparator = function standardComparator(obj, text) {
        text = ('' + text).toLowerCase();
        return ('' + obj).toLowerCase().indexOf(text) > -1;
    };

    return function customFilter(array, expression) {
        function customComparator(actual, expected) {


            var isLower = expected.lower;
            var isHigher = expected.higher;
            var higherLimit;
            var lowerLimit;


            if (angular.isObject(expected)) {

                if (isLower || isHigher) {
                    //number range
                    if (isLower) {
                        higherLimit = expected.lower;

                        if (actual > higherLimit) {
                            return false;
                        }
                    }

                    if (isHigher) {
                        lowerLimit = expected.higher;
                        if (actual < lowerLimit) {
                            return false;
                        }
                    }

                    return true;
                }
                return true;

            }
            return standardComparator(actual, expected);
        }

        var output = filterFilter(array, expression, customComparator);
        return output;
    };
}]);