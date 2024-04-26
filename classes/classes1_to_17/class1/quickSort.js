function quickSort(arr) {
    if (arr.length <= 1) {
        return arr;
      }
    
      let pivot = arr[0];
      let leftArr = [];
      let rightArr = [];
    
      for (let i = 1; i < arr.length; i++) {
        if (arr[i] < pivot) {
          leftArr.push(arr[i]);
        } else if (arr[i] > pivot) {
            rightArr.push(arr[i]);
          }
      }
    
      return [...quickSort(leftArr), pivot, ...quickSort(rightArr)];
  }
  
  // Example usage:
  const unsortedArray = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
  const sortedArray = quickSort(unsortedArray);
  console.log(sortedArray);
  