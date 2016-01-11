var list = [10, 123, 3, 1, 423, 2];


function qsort(list, lo, hi) {
    var p;
    if (lo < hi) {
        p = partition(list, lo, hi);
        qsort(list, lo, p - 1);
        qsort(list, p + 1, hi);
    }
    return list;
}

function swap(list, i, j) {
    var tmp = list[i];
    list[i] = list[j];
    list[j] = tmp;
}

function partition(list, lo, hi) {
    var pivot = list[hi];
    var i = lo;
    for (var j = lo; j <= hi - 1; j++) {
        if (list[j] <= pivot) {
            swap(list, i, j);
            i++;
        }
    }
    swap(list, i, hi);
    return i;
}

console.log(qsort(list, 0, list.length - 1));
