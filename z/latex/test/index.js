import fs from "fs";

function shuffleArray(array) {
  let m = array.length;
  let temp;
  let random;

  while (m) {
    random = Math.floor(Math.random() * m--);
    temp = array[m];
    array[m] = array[random];
    array[random] = temp;
  }

  return array;
}

function generateArray(n) {
  var arr = [];
  for (var i = 0; i <= n; i++) {
    arr.push(i);
  }
  return arr;
}

function generateSeriesOfArrays(numArrays, arrayLength) {
  var seriesOfArrays = [];

  for (var i = 0; i < numArrays; i++) {
    seriesOfArrays.push(shuffleArray(generateArray(arrayLength)));
  }

  return seriesOfArrays;
}


const iterations = 50;

var result = generateSeriesOfArrays(iterations, 50)

var data = {
  iterations: iterations,
  array: result,
};

var jsonData = JSON.stringify(data);

fs.writeFile("arrayData.json", jsonData, "utf8", function (err) {
  if (err) {
    console.log("An error occurred while saving the file:", err);
  } else {
    console.log("Array data saved successfully as arrayData.json");
  }
});
