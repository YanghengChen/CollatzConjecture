use std::env;


//iterative fucntion that returns sequence of passed integer
//--integer passed as parameter
fn find_sequence_recursed(num: i64) -> i64 {
   if num == 1 {
      return 0;
   } else if num%2 == 0{
      return 1 + find_sequence_recursed(num/2);
   } else {
      return 1 + find_sequence_recursed(3*num + 1);
   } 
}

//Main method
fn main() {
  //Taking command line input
  let args: Vec<String> = env::args().collect();
  let mut a1 : i64 = args[1].parse().unwrap();
  let mut a2 : i64 = args[2].parse().unwrap();
  
  //Declaring vectors
  let mut numbers : Vec<i64> = Vec::new();
  let mut seq_length : Vec<i64> = Vec::new();
  let mut sort_by_nums:[i64;10] = [0; 10];
  let mut sort_by_len:[i64;10] = [0; 10];
  
  //Swapping Varibles
  if a1 > a2 {
     let temp = a1;
     a1 = a2;
     a2 = temp; 
  }
  
  //Gathers sequences while in range
  for i in a1..a2 as i64 {
    let seq = find_sequence_recursed(i);
    numbers.push(i);
    seq_length.push(seq);
  }
  //Sorting by sequence length
  println!("Sorting by sequence length: ");
  bubble_sort(&mut seq_length, &mut numbers);
  let mut count = 0;
  let mut index = 0;
  while count < seq_length.len() && count < 10 {
    if numbers[index] == numbers[index+1] - 1 {
      println!("{}            {}",numbers[index], seq_length[index]);
      sort_by_nums[count] = numbers[index];
      sort_by_len[count] = seq_length[index];
      count = count + 1;
      index = index + 2;
    } else {
      println!("{}            {}",numbers[index], seq_length[index]);
      sort_by_nums[count] = numbers[index];
      sort_by_len[count] = seq_length[index];
      count = count + 1;
      index = index + 1;
    }
  }
  
  //Sorting the largest 10 values by the integer values
  println!("Sorting by integer size: ");
  bubble_sort(&mut sort_by_nums, &mut sort_by_len);
  let mut count = 0;
  while count < seq_length.len() && count < 10 {
    println!("{}            {}",numbers[count], seq_length[count]);
    count = count + 1;
   }
} //end main method


//Bubble Sort function to descending sort array of integers
//--both arrays passes as parameters and returned sorted
fn bubble_sort(length_arr: &mut[i64], num_arr: &mut[i64]) {
  for i in 0..length_arr.len() {
        for j in 0..length_arr.len() - 1 - i {
            if length_arr[j] < length_arr[j + 1] {
                length_arr.swap(j, j + 1);
                num_arr.swap(j, j + 1);
            }
        }
    }
}






