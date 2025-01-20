module Visualize

import vis::Graphs;
import Syntax;
import Content;
import IO;
import ListRelation;
extend lang::std::Id;

// Identity of a node
alias DepId = tuple[Id name, loc location];

alias DepGraph = lrel[DepId from, str kind, DepId to];


Content visualizeDeps(start[Form] form) = 
    graph(form2deps(form), 
        \layout=defaultCoseLayout(), 
        nodeLabeler=str (DepId d) { return "<d.name>"; }, 
        nodeLinker=loc (DepId d) { return d.location; });


DepGraph form2deps(start[Form] f) = form2deps(f.top);

// extra control/data dependencies from a form
// use the kind field in DepGraph to indicate whether it's a data dependency or a control dependency.
DepGraph form2deps(Form f){
    DepGraph deps = [];
    map[str, loc] idlocs = ();
    map[loc, Id] locsid = ();
    visit(f.questions){
        case (Id) `<Id name>`:{
            idlocs["<name>"] = name.src;
            locsid[name.src] = name;
        }
    }
    list[DepId] findDeps(Expr e){
        list[DepId] ids = [];
        visit(e){
            case (Id) `<Id name>`:{
                loc l = idlocs["<name>"];
                ids = ids + <locsid[l], l>;
            }
        }
        return ids;
    }

    void questionToDep(Question q, list[DepId] dependentOn){
        switch(q){
            case (Question)`if ( <Expr cond> ) <Question then>`:{
                questionToDep(then, dependentOn + findDeps(q.cond));
            }
            case (Question)`if (<Expr cond>) <Question then> else <Question elsethen>`:{
                questionToDep(then, dependentOn + findDeps(q.cond));
                questionToDep(elsethen, dependentOn);
            }
            case (Question)`{ <Question* questions> }`:{
                for(Question q <- questions){
                    questionToDep(q, dependentOn);
                }
            }
            default:{
                loc l = idlocs["<q.name>"];
                if (q is computed){
                    deps = deps + ([] | it + <d, "data", <locsid[l], l>> | DepId d <- findDeps(q.expr));
                }
                deps = deps + ([] | it + <d, "control", <locsid[l], l>> | DepId d <- dependentOn);
            }
        }
    }
    for (Question q <- f.questions){
        questionToDep(q, []);
    }


    return deps;
}

