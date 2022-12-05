#!/usr2/local/julia-1.8.2/bin/julia

# A global struct
struct Pair
   num::Int64     # An integer
   seqlen::Int64  # Collatz sequence length of the integer
end

function find_sequence_recursion(num)
   if num == 1
      return 0
   elseif num%2 == 0 
      return 1 + find_sequence_recursion(num/2)
   else
      return 1 + find_sequence_recursion(3*num + 1)
   end
end


function add_to_array(arr, elem)
   samelen = false
   # Adding first element
   if length(arr) == 0
      push!(arr, elem)
      # Adding until array has 10 elements
   elseif length(arr) < 10 
      for x in arr 
         if elem.seqlen == x.seqlen
            samelen = true
            break
         end 
      end
      if !samelen 
         push!(arr, elem)
      end 
      if length(arr) == 10
         sort!(arr, by = x -> x.seqlen, rev=true)
      end
   # When array is filled with 10 elements
   else 
      for x in arr
         if elem.seqlen == x.seqlen 
            samelen = true
            break
         end
      end
      if !samelen && elem.seqlen > arr[10].seqlen
         arr[10] = elem
         sort!(arr, by = x -> x.seqlen, rev=true)
      end        
   end
   return nothing
end

function main() 

  print("Start...\n")
  a1 = parse(Int64, ARGS[1])  #taking the first arguements  #First arguement
  a2 = parse(Int64, ARGS[2])  #taking the second argument   #Second Arguement
  
  #Swapping values
  if a1 > a2
    temp = a1
    a1 = a2
    a2 = temp
  end 
  
   numpairs = Array{Pair}(undef, 0) # Declare an empty vector of Pair structs       
   for num = a1:a2 
      apair = Pair(num, find_sequence_recursion(num))
      add_to_array(numpairs, apair)       
   end 
   println("Sorted based on sequence length") 
   sort!(numpairs, by = x -> x.seqlen, rev=true)     
   for x in numpairs
      println(lpad(x.num,21," "), lpad(x.seqlen,21," "))
   end
   println("Sorted based on integer size")
   sort!(numpairs, by = x -> x.num, rev=true)
   for x in numpairs
      println(lpad(x.num,21," "), lpad(x.seqlen,21," "))
   end
   return nothing
  
  

  print("End...\n")
end #end main function 

main()
exit()
