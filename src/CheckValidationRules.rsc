module CheckValidationRules

import Message;
import IO;
import ParseTree;
import List;
import String;
extend Syntax;

extend Check;

set[Message] check((Question) `<Str question> <Id _> : <Type anstype> <ValidationRule rule>`, TEnv env) {
    set[Message] messages = {};
    switch(anstype){
        case (Type)`boolean`:{
            if (rule is regex) messages += {error("Boolean questions cannot have a regex validation rule", rule.src)};
            if (rule is range) messages += {error("Boolean questions cannot have a range validation rule", rule.src)};
        }
        case (Type)`integer`:{
            if (rule is regex) messages += {error("Integer questions cannot have a regex validation rule", rule.src)};
        }
        case (Type)`string`:{
            if (rule is range) messages += {error("String questions cannot have a range validation rule", rule.src)};
        }   
    }
    switch(rule){
        case (ValidationRule)`[<Expr min> .. <Expr max> ]` : {
            messages += check(min, env);
            messages += check(max, env);

            messages += {error("Invalid min range", min.src) | (Type)`integer` !:= typeOf(min, env)};
            messages += {error("Invalid max range", max.src) | (Type)`integer` !:= typeOf(max, env)};
        }
    }
    return messages;
}