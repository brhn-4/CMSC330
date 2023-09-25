use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele);
        let mut new_add_i: usize = self.len()-1;
        let mut parent_i: usize;

        if self.len() == 1 {
            parent_i = 0;
        }
        else {
            parent_i = (new_add_i - 1) / 2;
        }
        while self[new_add_i] < self[parent_i] {
            self.swap(new_add_i, parent_i);
            new_add_i = parent_i;
            if new_add_i != 0 {
                parent_i = (new_add_i - 1) / 2;
            }
        }
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        let mut l_child: usize;
        let mut r_child: usize;
        if self.len() == 0{
            return None;
        }

        let dequeued = self.swap_remove(0);

        if self.len() == 0 {
            return Some(dequeued);
        }
        else if self.len() == 1 {
            return Some(dequeued);
        }
        else if self.len() == 2 {
            if self[0] > self[1] {
                self.swap(0, 1)
            }
        }
        else if self.len() == 3 {
            if self[0] > self[1] {
                self.swap(0, 1);
            }
            if self[0] > self[2] {
                self.swap(0, 2);
            }
        }
        else{
            let mut index: usize = 0;
            while index < ((self.len() -1) /2){
                l_child = 2 * index + 1;
                r_child = 2 * index + 2;
                if r_child < self.len() && l_child < self.len() {
                    if l_child >= r_child {
                        self.swap(index, r_child);
                        index = r_child;
                    }
                    else{
                        self.swap(index,l_child);
                        index = l_child;
                    }
                }
                else if l_child == self.len(){
                    self.swap(index, l_child);
                    index = l_child;
                }
            }
        }
        return Some(dequeued)
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0{
            None
        }
        else{
            return Some(&self[0]);
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut visited = Vec::new();
    let mut queue = Vec::new();

    for(ally,a_coords) in allies.iter(){
        for (enemy, e_coords) in enemies.iter(){
            let dist = distance(*a_coords, *e_coords);
            let new_node = Node {priority: dist, data: (ally,enemy)};
            queue.enqueue(new_node);
        }
    }

    let mut not_done = true;

    while not_done {
        if queue.peek().unwrap().data.0 == &&"Stark".to_string() && visited.contains(queue.peek().unwrap().data.1) == false{
            not_done = false;
        }
        else{
            visited.push(queue.peek().unwrap().data.1);
            queue.dequeue();
        }
    }
    let enemy_name = queue.peek().unwrap().data.1;
    let coords = enemies.get(enemy_name).unwrap();
    return (enemy_name, coords.0,coords.1);
 } 


