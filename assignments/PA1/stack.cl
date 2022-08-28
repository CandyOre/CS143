(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class List {

   val : String;
   nxt : List;

   val() : String { val };
   nxt() : List { nxt };

   init (val_ : String, nxt_ : List) : List {{
      val <- val_;
      nxt <- nxt_;
      self;
   }};
};

class StackMachine {

   run : Bool;
   list : List;

   alive() : Bool { run };
   list() : List { list };

   run() : StackMachine {{
      run <- true;
      self;
   }};

   stop() : StackMachine {{
      run <- false;
      self;
   }};

   push (x : String) : StackMachine {{
      list <- (new List).init(x, list);
      self;
   }};

   pop() : String {
      if isvoid list then
         ""
      else 
         let
            val : String <- list.val()
         in {
            list <- list.nxt();
            val;
         }
      fi
   };
};

class StackCommand {

   from (line : String) : StackCommand {
      if line = "x" then new ExitCommand else
      if line = "e" then new EvalCommand else
      if line = "d" then new DisplayCommand else
      (new PushCommand).with(line) fi fi fi
   };

   eval (machine : StackMachine) : StackMachine { machine };
};

class EvalCommand inherits StackCommand {

   add (machine : StackMachine) : StackMachine {
      let
         helper : A2I <- new A2I,
         item1 : Int <- helper.a2i(machine.pop()),
         item2 : Int <- helper.a2i(machine.pop())
      in
         machine.push(helper.i2a(item1 + item2))
   };

   swap (machine : StackMachine) : StackMachine {
      let
         item1 : String <- machine.pop(),
         item2 : String <- machine.pop()
      in
         machine.push(item1).push(item2)
   };

   eval (machine : StackMachine) : StackMachine {
      let
         top : String <- machine.pop()
      in
         if top = "+" then self.add(machine) else
         if top = "s" then self.swap(machine) else
         if top = "" then machine else 
         machine.push(top) fi fi fi
   };
};

class DisplayCommand inherits StackCommand {

   eval (machine : StackMachine) : StackMachine {{
      let
         cur : List <- machine.list(),
         io : IO <- new IO
      in
         while not isvoid cur loop {
            io.out_string(cur.val().concat("\n"));
            cur <- cur.nxt();
         } pool;
      machine;
   }};
};

class ExitCommand inherits StackCommand {

   eval (machine : StackMachine) : StackMachine { machine.stop() };
};

class PushCommand inherits StackCommand {

   line : String;

   with (line_ : String) : PushCommand {{
      line <- line_;
      self;
   }};

   eval (machine : StackMachine) : StackMachine {
      machine.push(line)
   };
};

class Main inherits IO {

   main() : Object {{
      -- out_string("Welcome to Stack Machine!\n");
      let
         machine : StackMachine <- (new StackMachine).run()
      in
         while machine.alive() loop {
            out_string(">");
            machine <- (new StackCommand).from(in_string()).eval(machine);
            -- out_string(machine.list().val());
         } pool;
   }};
};
