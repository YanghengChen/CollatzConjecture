package main

import "fmt"
import "sort"  
import "os"
import "strconv"



// Finds and returns the collatz sequence length of an integer.
func find_sequence_recursion(num int) int {
  if num == 1 {
    return 0
  } else if num%2 == 0 {
    return 1 + find_sequence_recursion(num/2)
  } else {
    return 1 + find_sequence_recursion(3*num + 1)
  }
}

func main() {
    //Getting Command Line Arguements
    a1, err := strconv.Atoi(os.Args[1])   //First  
    a2, err := strconv.Atoi(os.Args[2])   //Second
    
    
    if  err != nil  { 
      fmt.Printf("Invalid range and/or arguments!")
      return
    }
    
    arg1 := int(a1)
    arg2 := int(a2)
    var temp int = 0
    if(arg1 > arg2){
        temp = arg1
        arg1 = arg2
        arg2 = temp
    }
    
    m := make(map[int]int)
     
    for i := arg1; i <= arg2; i++ {
    // Adding the first entry
    if len(m) == 0 {                              
      m[find_sequence_recursion(i)] = i
    // Adding another entry if the same sequence length does not exist
    } else if len(m) < 10 {
      if _, ok := m[find_sequence_recursion(i)]; !ok {
        m[find_sequence_recursion(i)] = i 
      }
    // Map is maintained at a maximum size of 10
    // Updating entries with ones having greater but different sequence lengths
    } else {

      var seqlens []int
      for k := range m {
        seqlens = append(seqlens, k)
      }
      sort.Slice(seqlens, func(i, j int) bool {
        return seqlens[i] > seqlens[j]
      })

      if _, ok := m[find_sequence_recursion(i)]; !ok {
        if find_sequence_recursion(i) > seqlens[9] {
	  delete(m, seqlens[9])
	  m[find_sequence_recursion(i)] = i 
        }
      }
    }
  }
  
  var keys []int
  for k := range m {
    keys = append(keys, k)
  }

  sort.Slice(keys, func(i, j int) bool {
    return keys[i] > keys[j]
  })
  fmt.Println("Sorted based on sequence length")
  for _, elem := range keys {
    fmt.Printf("%21d%21d\n", m[elem], elem)
  }
  
  sort.SliceStable(keys, func(i, j int) bool {
    return m[keys[i]] > m[keys[j]]
  }) 
  fmt.Println("Sorted based on integer size")
  for _, elem := range keys {
    fmt.Printf("%21d%21d\n", m[elem], elem)
  }
}