require_relative "graph.rb"
#Brandon Nguyen - 116621335
#CMSC 330
#Van Horn
class Synsets
    def initialize
        @map = {}
        nil
    end

    def load(synsets_file)
        invalid_arr = []
        index = 1
        hash = {}

        File.open(synsets_file, "r") do |file|
            for line in file.readlines
                split_arr = line.split(/\s+/)
                id = split_arr[1].to_i

                if !(line =~ /^id: (\d+) synset: ([\w\-\/\.\']+(,[\w\-\/\.\']+)*)/) then
                    invalid_arr.push(index)
                    
                elsif  @map.has_key?(id) || split_arr[3].empty?|| id < 0|| hash.has_key?(split_arr[1].to_i)
                    invalid_arr.push(index)
                else    
                    hash[id] = split_arr[3].split(",")     
                end
                index += 1
            end
        end

        if  !(invalid_arr.empty?) then
            return invalid_arr
        else
            hash.keys.each{ |key|
                addSet(key,hash[key])    
            }
            return nil
        end

    end

    def addSet(synset_id, nouns)
        if(synset_id >= 0 && !(nouns.empty?) && !(@map.has_key?(synset_id))) then
            @map[synset_id] = nouns
            return true
        end

        return false
    end

    def lookup(synset_id)
        if !(@map.has_key?(synset_id)) then
            return []
        end

        return @map[synset_id]  
    end

    def findSynsets(to_find)
        if to_find.kind_of?(Array) then
            ret_hash = {}
            for elem in to_find
                ret_hash[elem] = []
                for key in @map.keys
                    if(@map[key].include?(elem))
                        ret_hash[elem].push(key)
                    end
                end
            end
            return ret_hash
        elsif to_find.kind_of?(String) then
            ret_array = []
            for key in @map.keys   
                for elem in @map[key]
                    if(to_find == elem)
                        ret_array.push(key)
                    end
                end
            end
            return ret_array
        else
            return nil
        end
    end
end

class Hypernyms
    def initialize
        @graph = Graph.new
    end

    def load(hypernyms_file)
        invalid_arr = []
        index = 1
        

        File.open(hypernyms_file, "r") do |file|
            for line in file.readlines
                temp_arr = line.split(/\s+/)

                if(line =~ /^from: (\d+) to: ([0-9]+(,[0-9]+)*)/) then
                    index += 1
                else
                    invalid_arr.push(index)
                    index += 1
                end
            end
        end
        if(!(invalid_arr.empty?)) then
            return invalid_arr
        else
            for line in File.readlines(hypernyms_file)
                correct_arr = line.split(/\s+/)
                source = correct_arr[1].to_i
                destination = correct_arr[3].to_i
                addHypernym(source,destination)
            end
            return nil
        end
        
    end

    def addHypernym(source, destination)
        if(source >= 0 && destination >= 00 && destination != source) then
            if !(@graph.hasVertex?(source)) then
                @graph.addVertex(source)
            end 

            if !(@graph.hasVertex?(destination)) then
                @graph.addVertex(destination)
            end

            if !(@graph.hasEdge?(source,destination)) then
                @graph.addEdge(source,destination)
            end

            return true
        else
            return false
        end
    end

    def lca(id1, id2)
        if(!(@graph.hasVertex?(id1)) || !(@graph.hasVertex?(id2)))
            return nil
        end
        min_arr = Array.new
        map1 = @graph.bfs(id1)
        map2 = @graph.bfs(id2)
        distance_map = Hash.new
        min = 999999999 #arbitrarily large

        for key1 in map1.keys
            for key2 in map2.keys
                if(key1 == key2) then
                    distance_map[key1] = map1[key1] + map2[key2]
            
                end
            end
        end

        for key3 in distance_map.keys
            if(distance_map[key3] < min) then
                min = distance_map[key3]
            end
        end

        for key4 in distance_map.keys
            if(distance_map[key4] == min) then
                min_arr.push(key4)
            end
        end
        return min_arr

    end
end
class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

    def parse(command)
        hash = Hash.new
        comm_arr = command.split(/\s+/)
        comm = comm_arr[0]
        if comm == "load" then
            hash[:recognized_command] = :load
            if(comm_arr.length != 3) then
                hash[:result] = :error
            else
                if(@synsets.load(comm_arr[1]) == nil && @hypernyms.load(comm_arr[2]) == nil) then
                    hash[:result] = true
                else
                    hash[:result] = false
                end
            end
        elsif comm == "find" then
            hash[:recognized_command] = :find
            if comm_arr.length != 2 then
                hash[:result] = :error
            else
                hash[:result] = @synsets.findSynsets(comm_arr[1])
            end
        elsif comm == "lca" then
            hash[:recognized_command] = :lca
            if(comm_arr != 3) then
                hash[:result] = :error
            else
                hash[:result] = @hypernyms.lca(comm_arr[1].to_i,comm_arr[2].to_i)
            end
        elsif comm == "findmany" then
            hash[:recognized_command] = :findmany
            if(comm_arr.length != 2)
                hash[:result] = :error
            else
                comm_arr.shift
                comm_arr = comm_arr[0].split(",")
                hash[:result] = @synsets.findSynsets(comm_arr)
            end
        elsif comm == "lookup" then
            hash[:recognized_command] = :lookup
            if(comm_arr.length != 2) then
                hash[:result] = :error
            else
                hash[:result] = @synsets.lookup(comm_arr[1].to_i)
            end
        else
            hash[:recognized_command] = :invalid
        end
        return hash
    end
end