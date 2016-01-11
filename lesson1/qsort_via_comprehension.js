var result = "";

function qsort(arr) {
  if (arr.length == 0) {
    return [];
  }

  var pivot = arr[0];
  var arrWithoutPivot = arr.slice(1);
  
  var lesserPart = [for (i of arrWithoutPivot) if (i < pivot) i];
  var biggerPart = [for (i of arrWithoutPivot) if (i >= pivot) i];

  return qsort(lesserPart).concat([pivot]).concat(qsort(biggerPart));
}

var list = [10, 123, 3, 1, 5];

console.log(qsort(list));
