(* Copy this skeleton to a file called "Type.sml" and modify it *)
(* Compile by mosmlc -c Type.sml *)
(* Then recompile Main by mosmlc Main.sml -o Main *)

(* You will need to add more functions *)
(* as well as modifying those shown *)


structure Type :> Type =
    struct
    
    (* Use "raise Error (message,position)" for error messages *)
	exception Error of string*(int*int)
	
	(* lookup function for symbol table as list of (name,value) pairs *)
	fun lookup x []
	    = NONE 
	  | lookup x ((y,v)::table) 
	    = if x=y then SOME v else lookup x table
      
	fun bind [] x y = ((x,y)::[]) 
	  | bind table z v = ((z,v)::table) 
	    (* getlist[] benyttes til kommentarer *)
	    
	    
	(* main function for checking program *)
	fun check (decls,body) = (*print (";starter compiling") before*) 
	    let
	  val ftable = checkDecls decls []
	    in
		checkBody body [] ftable
	    end



	(* check function/procedure declarations and return ftable *)
	and checkDecls [] ftable =  ftable 
	  | checkDecls (Rascal.Procedure (name,pars,body,pos)::decls) ftable =
	    if not(isSome (lookup ("p"^name) ftable)) 
		then checkDecls decls (bind ftable ("p"^name) (checkVar pars [] )) 
	    else raise Error("Funktionsnavn eksisterer allerede", pos)
		
	  | checkDecls (Rascal.Function (name,pars,body,pos)::decls) ftable =
		if not(isSome (lookup ("f"^name) ftable) ) 
 		    then  checkDecls decls (bind ftable ("f"^name) (checkVar pars [] )) 
		else raise Error("Procedurenavn eksisterer allerede", pos)
		    
	(* **checkBody** - tjekker Vars og Stats med henholdsvis checkVar og checkStat *)	      
	and checkBody (vars,stats) vtable ftable = 
	     let  val v1 = checkVar vars vtable
 		       val s1 = foldl (fn (g,h) => checkStat g h ftable) v1 stats
					  in ()
					  end
				      
	(* **checkVar** - tjekker liste af variable (VarDecls) og binder dem i vtable *)
	and checkVar [] vtable = vtable  
	  | checkVar ((name,typ,pos)::vars) vtable =
	    if not(isSome (lookup name vtable)) 
		then checkVar vars (bind vtable name typ) 
	    else raise Error("Variabelnavn er allerede defineret andetsteds", pos)
	  
	(* ---------------------------------------* 		
	 * check statement - vi returnerer vtable *
	 * -------------------------------------- *) 
	and checkStat stat vtable ftable =  
	    case stat of
		Rascal.PCall (name,args,pos) => 
		    let val pro = lookup ("p"^name) ftable
		    in
			if (isSome pro) 
			    then 
				let val x = 
				    hd (map (fn g => checkExp g vtable ftable) args) 
				in 
				    if x = Rascal.Integer 
					orelse x = Rascal.Array(0) 
					then vtable 
				    else raise Error ("Fejl ved procedurekald : "^name^", forkert(e) type(r)",pos)
				end
			else
			raise Error ("Fejl ved Procedurekald, manglende defintione :"^name,pos)
		    end
	      | Rascal.Assign (lval,e,pos) => 
			    let val t1 = checkExp e vtable ftable
				val t2 = checkLVal lval vtable ftable
				val vtable2 = bind vtable (getNameLval lval) t1 
			    in 
				if (t1 = Rascal.Integer orelse t1 = Rascal.Array(0)) 
				    andalso (t2 = Rascal.Integer orelse t2 = Rascal.Array(0))
				    then vtable2 
				else 
				    if  not(t1=Rascal.Integer) andalso not(t2=Rascal.Integer) 
					then 
					    if getArraySize(t1) = getArraySize(t2) 
						then vtable2
					    else
						raise Error ("Arrays har ikke samme atributter ",pos)
				    else raise Error ("Fejl ved :=, forkert(e) type(r) ",pos)
			    end
	      | Rascal.IfThen (c,s,pos) => 
			    let val c1 = checkCond c vtable ftable
				val s1 = checkStat s vtable ftable
			    in if c1 = "condition" 
				   then s1
			       else raise Error("Fejl ved If-sÊtning, sammenligning mangler",pos) 
			    end
			
	      | Rascal.IfThenElse (c,s1,s2,pos) => 
			    let val c1 = checkCond c vtable ftable
				val s3 = checkStat s1 vtable ftable
				val s4 = checkStat s2 s3 ftable
						   in if c1 = "condition" 
							  then s4
						      else raise Error("Fejl ved If-sÊtning, sammenligning mangler",pos) 
			    end
			
	      | Rascal.While (c,s,pos) => 
			    let val c1 = checkCond c vtable ftable
				val s1 = checkStat s vtable ftable
			    in if  c1 = "condition"  
				   then s1
			       else raise Error("Fejl i while-l¯kke, sammenligning mangler",pos) 
			    end
			
	      | Rascal.Repeat (stats,c,pos) => 
			    let val s1 = foldl (fn (g,h) => checkStat g h ftable) vtable stats
				val c1 = checkCond c vtable ftable
			    in if c1 = "condition"  
				   then s1
			       else raise Error("Fejl ved Repeat, sammenligning mangler",pos)
			    end
			
	      | Rascal.Read (lval,pos) =>  
			    let val lv = checkLVal lval vtable ftable  
			    in (* if lv = Rascal.Integer before print(";loop"^getNameLval(lval)^"\n")
				    then*) vtable 
				(*else raise Error("Fejl ved read, forkert type",pos)*)
			    end
			
	      | Rascal.Write (e,pos) => 
			    let val e1 = checkExp e vtable ftable
			    in  (*if e1 = Rascal.Integer(*Ligegyldigt tal *) 
				    then*) vtable 
				(*else raise Error("Fejl i write, forkert type",pos)*)
			    end
	      | Rascal.Block stats =>  
			    foldl (fn (g,h) => checkStat g h ftable) vtable stats 
			 
					
	(* *****************  *
	 *  Slut pÂ checkStat *
	 *******************  *)			     
			    
	(* --------------------------------------------------------------------- * 
	 * smÂfunktioner - getNameLval, getTypeLval, getArraySize, checkArrayExp *
 	 * --------------------------------------------------------------------- *)
					
	and getNameLval lval = 
	    case lval of
		Rascal.Id(s1,pos) =>  s1
	| Rascal.ArrayElem(s1,args,pos) => s1

(* getTypeLval bruges umiddelbart kun til print, skal slettes *)	
	and getTypeLval lval =
	    case lval of
		Rascal.Id(s1,pos) =>  "id"
	      | Rascal.ArrayElem(s1,args,pos) => "array"
		    
	and getArraySize(Rascal.Array(a)) = a
	  | getArraySize _ = 0
 
	and checkArrayExp e =
	    case e of
		Rascal.IConst (n,pos) => n
	      | Rascal.LVal lval    => let val f = getTypeLval(lval)
				       in raise Fail f end
	      | _		     => raise Fail "Andet en integer i Array"
					   
					   
					   
	(* Tjek LVal - unders¯ger om LVal er int eller arrayelement *)
	and checkLVal l vtable ftable = 
	    case l of 
		Rascal.Id(s1,pos) =>  
		    if (isSome (lookup s1 vtable))  
			then valOf(lookup s1 vtable)
		    else  raise Error ("Syntax fejl",pos)
	      | Rascal.ArrayElem(s1,args,pos) =>  
			if (isSome (lookup s1 vtable)) 
			    then 
				let val e = checkExp args vtable ftable 
				in if not(valOf(lookup s1 vtable)=Rascal.Integer)andalso e = Rascal.Integer then valOf(lookup s1 vtable) else raise Error ("Denne variabel: "^s1^" har forkert type",pos)(*Rascal.Integer*) end
			       
			else  raise Error ("Syntax fejl",pos)	
						      
	(* slut checkLVal *)

	(* ---------------------------------- * 
	 * check Expression - Returnerer Type *
	 * ---------------------------------- *) 				    

	and checkExp e vtable ftable  = 
	    case e of
		Rascal.IConst (n,pos)   => 
		    Rascal.Integer
	      
	      | Rascal.LVal lval  	  =>  
		    checkLVal lval vtable ftable
		    
	      | Rascal.Plus (e1,e2,pos) =>  
		    if (checkExp e1 vtable ftable = Rascal.Integer) andalso
			(checkExp e2 vtable ftable = Rascal.Integer) 
			then Rascal.Integer 
		    else
			raise Error ("Fejl ved plus, forkerte parametre",pos)
	      | Rascal.Minus (e1,e2,pos) => 
			    if (checkExp e1 vtable ftable = Rascal.Integer) andalso
				(checkExp e2 vtable ftable = Rascal.Integer)
				then Rascal.Integer 
			    else
				raise Error ("Fejl ved minus, forkerte parametre",pos) 
	      | Rascal.UMinus (e1,pos) => 
				    if (checkExp e1 vtable ftable = Rascal.Integer) 
					
					then Rascal.Integer 
				    else
					raise Error ("Fejl ved minus, forkerte parametre",pos)	
	      | Rascal.FCall (name,args,pos) => 
					    let val t1 = lookup ("f"^name) ftable 
					    in if (isSome t1)
						   then hd(rev (map (fn g => checkExp g vtable ftable) args))
					       else 
						   raise Error ("Fejl ved funktionskald (FCall)",pos) 
						  end
					    
	(* ***************** *
	 *  Slut pÂ checkExp *
	 ******************* *)			     

	(* ------------------------------------------------- *      
	 * check condition - Returnerer strengen "condition" *
	 * ------------------------------------------------- *)
	and checkCond c vtable ftable =
	    case c of
		Rascal.EQ (e1,e2,pos) => 
		    let  val t1 = checkExp e1 vtable ftable 
			val t2 = checkExp e2 vtable ftable 
		    in if t1 = t2 
			   then ("condition") 
		       else raise Error ("Fejl ved sammenligning (=), forkerte parametre",pos) 
		    end
	      | Rascal.LT (e1,e2,pos) => 
		    let  val t1 = checkExp e1 vtable ftable 
			val t2 = checkExp e2 vtable ftable 
		    in if t1 = t2 
			   then ("condition") 
		       else raise Error ("Fejl ved sammenligning, (<) forkerte parametre",pos) 
		    end
		
	      | Rascal.AND (c1,c2,pos) => 
		    let  val t1 = checkCond c1 vtable ftable 
			val t2 = checkCond c2 vtable ftable in
			    if (t1=t2) 
				then ("condition") 
			    else raise Error ("Fejl ved sammenligning (and), forkerte parametre",pos) 
		    end
		
	      | Rascal.OR (c1,c2,pos) => 
		    let  val t1 = checkCond c1 vtable ftable 
			val t2 = checkCond c2 vtable ftable 
		    in
			if (t1=t2) 
			    then ("condition") 
			else raise Error ("Fejl ved sammenligning (or), forkerte parametre",pos) 
		    end
		
	      | Rascal.NOT (c1,pos) => 
		    let val cny = checkCond c1 vtable ftable
		    in if (cny = "condition") 
			   then ("condition") 
		       else raise Error ("Fejl ved not, forkert parameter",pos) 
		    end
		
    (* ****************** *
     *  Slut pÂ checkCond *
     ******************** *)			     
  
    end
