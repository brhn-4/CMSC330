#Brandon Nguyen - 116621335
#9/11/2020
#CMSC 330 Van Horn


# First let's do some general
# code block practice

# Given a hash, return
# all values that are divisble
# by 2. Assume values are ints
# Use a code block.

def evens_string(hash)
    ret_str = ""
    hash.each do |k,v|
        if (v%2 == 0) then
            ret_str = ret_str + v.to_s
        end
    end
    ret_str

end

# Now let's up the stakes
# Given an array of elements
# return a new array whose
# elements have been processed
# by the code block

# If no code block is given,
# simply return an array where
# every element is increased by 1
def map_w_code_block(arr)
    if block_given? then
       return arr.collect{|x| yield x}
    else
        index = 0
        for elem in arr
            arr[index] = elem + 1
            index = index + 1
        end
        return arr
    end
    
end


# Time for some regex practice!
# Write a regular expression to
# capture a time.

# Times are defined in the following
# way:
# A 2 digit hour (from 01 to 12)
# A 2 digit minute (from 00 to 59)
# A 2 digit second (from 00 to 59)
# A two letter indication : A.M., P.M.
# EX. 12:31:59 P.M.

# If I give you a valid time:
# return the string "It is _ _",
# where the first blank is replaced
# by the hour, and the second blank
# is replaced by A.M. or P.M.
# EX. "It is 12 P.M." (It doesn't
# matter if it's 12:59:59 - It's still
# 12 for us)
# If I give you ANYTHING else
# return the string "Invalid"
#/^(0[0-9]|10|11|12):[0-5][0-9]:[0-5][0-9] (P\.M\. | A\.M\.)$/
def time_teller(time_str)
    if time_str =~ /^(0[0-9]|10|11|12):[0-5][0-9]:[0-5][0-9] (P\.M\.|A\.M\.)$/ then
        hours = $1
        am_or_pm = $2
        return "It is #{hours} #{am_or_pm}"
    else
        return "Invalid"
    end
end


# Alright, we've got three basic
# exercises out of the way, let's
# put it all together!

class Grader

    # You'll be given a file of strings
    # Each line has a first name and last name
    # (Start with capital, then lowercase),
    # followed by a comma and then their grade
    # a number from 0 to 100
    # EX. "Frodo Baggins, 98"
    def initialize(filename)
        # initialize some relevant data
        # structure here
        @student_hash = {}
        File.foreach(filename) do |line|
            if (line =~ /^(\w+\s\w+), ([0-9]|100|[0-9][0-9])$/) then
                name = $1
                grade = $2.to_i
                @student_hash[name] = grade
            end
        end
    end

    # Because 330 is so great,
    # we'll sometimes spontaneously
    # give all students some extra
    # credit, defined by a code block
    # we pass in. Update your data
    # to add this extra credit
    def add_extra_credit()
        @student_hash.each do |k,v|
            @student_hash[k] = yield @student_hash[k]
        end
    end

    # Return the grade for the
    # specified student
    def get_grades_for_student(student_Name)
        return @student_hash[student_Name]
    end
end
