/**
    BRANDON NGUYEN
    CMSC330
    VAN HORN
    12/8/2020
**/

/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1;
    }
    else{
        let mut sum = 0;
        for v in 0..n {
            sum+= v;
        }
        sum+= n;
        return sum;

    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut cnt = 0;
    
    for index in 0..ls.len(){
        if ls[index] >= s && ls[index] <= e{
            cnt+= 1;
        }
    }
    return cnt;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    if target.len() > set.len(){
        return false;
    }
   let mut ret_bool = true;
   for elem in target.iter(){
                    let mut in_set = false;
                    for elem2 in set.iter(){
                                        if elem == elem2 {
                                            in_set = true;
                                        }
                    }
                    if in_set == false{
                       ret_bool = false;
                        
                    }
                 }
    return ret_bool;
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 {
		return None;
	} else {
		return Some((ls.iter().fold(0.0,|a,b| a + b)) / ls.len() as f64);
	}
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut total: i32 = 0;
    let mut index: u32  = ls.len()  as u32;
    let two: i32 = 2;    
    for x in 0..ls.len() {
        index -= 1;
        if ls[x] == 1{
        total += two.pow(index);
        }
        
    }

    return total; 

   
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut i = 2;
    let mut val = n;
    let mut new_vec = Vec::new();

    while i <= n{
        if val%i == 0{
            new_vec.push(i);
            val = val/i;
        }
        else{
            i += 1;
        }
    }
    return new_vec;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut new_vec = Vec::new();
    if lst.len() == 0 {
        return new_vec;
    }

    for i in 1..lst.len() {
        new_vec.push(lst[i]);
    }

    let y:i32 = lst[0];
    new_vec.push(y);
    return new_vec;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let s1 = s.as_bytes();
    let t1 = target.as_bytes();
    subset(s1, t1)


  
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    let s_arr = s.as_bytes();
    let mut first = 0;
    let mut last = 0;
    let mut cnt = 0;

    if s_arr.len() == 0{
        return None;
    }

    for start_index in 0..s_arr.len(){
        let mut curr = 1;
        let mut end_index = start_index;

        for i in start_index..s_arr.len(){
            if s_arr[start_index] != s_arr[i]{
                break;
            }
            curr += 1;
            end_index = i +1;
        }

        if curr>cnt{
            cnt = curr;
            first = start_index;
            last = end_index;
        }
    }
    let ret = &s[first..last];
    return Some(&ret);
 
  
}
