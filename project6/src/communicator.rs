#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        match self {
            Command::Power(ref change,ref val) => {
                                                if *change == true {
                                                     return format!("Power increased by {:?}%", val);
                                                }
                                                else{
                                                    return format!("Power decreased by {:?}%", val);   
                                                }
                                            },
            Command::Missiles(ref change1,ref num) => { 
                                                    if *change1 == true {
                                                        return format!("Missiles increased by {:?}", num);
                                                    }
                                                    else{
                                                        return format!("Missiles decreased by {:?}", num);  
                                                    }
                                                },
            Command::Shield(ref status) => { 
                                            if *status == true {
                                                return format!("Shield turned on");
                                            }
                                           else{
                                                 return format!("Shield turned off");
                                            }
                                        },
            Command::Try => { 
                return format!("Call attempt failed");
            },
            Command::Invalid => {
                return format!("Not a command");
            }
        }
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
     let command: Vec<&str> = s.split_whitespace().collect();

   if command.len() == 3 && command[0] == "power" && (command[1] == "inc" || command[1] == "dec") && (command[2].trim() == command[2])
   && command[2].parse::<i32>().is_ok(){
       if command[1] == "inc"{
           return Command::Power(true,command[2].trim().parse::<i32>().unwrap());
       }
       else{
        return Command::Power(false,command[2].trim().parse::<i32>().unwrap());
       }
   }

   else if command.len() == 3 && (command[0] == "fire" || command[0] == "add") && command[1].trim() == command[1] &&
    command[1].parse::<i32>().is_ok() && command[2] == "missiles" {
    
        if command[0] == "add" {
            return Command::Missiles(true,command[1].parse::<i32>().unwrap());
        }
        else{
            return Command::Missiles(false,command[1].parse::<i32>().unwrap());
        }
    }
    else if command.len() == 2 && command[0] == "shield" && (command[1] == "on" || command[1] == "off") {
        if command[1] == "off" {
            return Command::Shield(false);
        }
        else {
            return Command::Shield(true);
        }
    }

    else if command.len() == 4 && command[0] == "try" && command[1] == "calling" && command[2] == "Miss" && command[3] == "Potts" {
        return Command::Try;
    }
    // Invalid (anything else)
    else {
        return Command::Invalid;
    }
}