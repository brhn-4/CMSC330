# We will be implimenting a simple database table using Ruby data structures to store the data.
# The class Tuple represents an entry in a table.
# The class Table represents a collection of tuples.

class Tuple

    @@numCount = Hash.new(0)
    # data is an array of values for the tuple
    def initialize(data)
        @data = data
        @@numCount[@data.length] = @@numCount[@data.length] + 1
    end

    # This method returns the number of entries in this tuple
    def getSize()
        return @data.length
    end

    # This method returns the data at a particular index of a tuple (0 indexing)
    # If the provided index exceeds the largest index in the tuple, nil should be returned.
    # index is an Integer representing a valid index in the tuple.
    def getData(index)
        @data.at(index)
    end

    # This method should return the number of tuples of size n that have ever been created
    # hint: you should use a static variable
    # hint2: a hash can be helpful (though not strictly necessary!)
    def self.getNumTuples(n) 
        return @@numCount[n]
    end
end

class Table
    # column_names is an Array of Strings
    def initialize(column_names)
        @tuple_arr = []
        @column_names = column_names
        @column_hash = {}
        for name in @column_names
            @column_hash[name] = []
        end

        @tuple_cnt = 0
    end

    # This method inserts a tuple into the table.
    # Note that tuples inserted into the table must have the right number of entries
    # I.e., the tuple should be the size of column_names
    # If the tuple is the correct size, insert it and return true
    # otherwise, DO NOT insert the tuple and return false instead.
    # tuple is an instance of class Tuple declared above.
    def insertTuple(tuple)
        if tuple.getSize() != @column_names.length
            return false
        end
        index = 0
        for name in @column_names
            @column_hash[name][@tuple_cnt] = tuple.getData(index)
            index = index + 1
        end
        @tuple_arr.push(tuple)
        @tuple_cnt = @tuple_cnt + 1

        return true
    end
    
    # This method returns the number of tuples in the table
    def getSize
        @tuple_cnt
    end

    # This method selects columns from the table, equivalent to a SQL `select column_names from table` query.
    # This should return a new table that is identical in structure to this one,
    # except only including the columns listed in the `column_names` array
    # EXAMPLE:
    #  column_1 | column_2 | column_3 | column_4
    # -------------------------------------------
    #     1     |    2     |     3    |     4    
    #     2     |    3     |     4    |     1    
    #     3     |    4     |     1    |     2      
    #     4     |    1     |     2    |     3    
    # Note that this is a table made up of 4-element tuples
    # selectTuples(["column_1", "column_3"]) should return a table with the structure
    #  column_1 | column_3
    # ---------------------
    #     1     |     3    
    #     2     |     4    
    #     3     |     1      
    #     4     |     2    
    # Notice that we NOW have a table of 2-element tuples
    # hint: to find the index of an element in an array, you can use arr.index(element)
    def selectTuples(column_names)
        new_table = Table.new(column_names)
        index = self.getSize
        for tup in @tuple_arr
            
            temp_data = []

            for name in column_names 
                temp_data.push(tup.getData(@column_names.index(name)))           
            end
            temp_tuple = Tuple.new(temp_data)
            new_table.insertTuple(temp_tuple)
        end
        return new_table

    end

    # This should return an array of the tuples present in the table
    def getTuples()
        @tuple_arr
    end
end