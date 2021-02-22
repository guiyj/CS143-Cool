(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Command {
   execute(s: Stack): Stack {
      s
   };
   print(): IO {
      (new IO).out_string("error: forbidden command\n")
   };
};

class IntCommand inherits Command {
   data: Int;

   set(i: Int): IntCommand {
      {
         data <- i;
         self;
      }
   };
   get(): Int {
      data
   };
   print(): IO {
      let io: IO <- new IO in
         if isvoid data
         then io.out_string("error: uninitialized IntCommand\n")
         else let cvt: A2I <- new A2I in
            io.out_string(cvt.i2a(data).concat("\n"))
         fi
   };
};

class AddCommand inherits Command {
   print(): IO {
      (new IO).out_string("+\n")
   };
   -- assume at least 2 IntCommands in the Stack
   execute(s: Stack): Stack {
      let cmd1: Command <- s.top() in
         case cmd1 of
            lhs: IntCommand => {
               s <- s.pop();
               let cmd2: Command <- s.top() in 
                  case cmd2 of
                     rhs: IntCommand =>
                        s.pop().push(new IntCommand.set(lhs.get() + rhs.get()));
                     others: Command => s.push(cmd1);
                  esac;
            };
            others: Command => s;
         esac
   };
};

class SwapCommand inherits Command {
   print(): IO {
      (new IO).out_string("s\n")
   };
   -- assume at least 2 commands in the Stack
   execute(s: Stack): Stack {
      let cmd1: Command <- s.top() in {
         s <- s.pop();
         let cmd2: Command <- s.top() in
            s.pop().push(cmd1).push(cmd2);
      }
   };
};

class EvalCommand inherits Command {
   print(): IO {
      (new IO).out_string("e\n")
   };
   execute(s: Stack): Stack {
      if s.empty()
      then s
      else let cmd: Command <- s.top() in
         case cmd of
            add: AddCommand => s <- add.execute(s.pop());
            swap: SwapCommand => s <- swap.execute(s.pop());
            int: IntCommand => s;
            others: Command => s;
         esac
      fi
   };
};

class DisplayCommand inherits Command {
   print(): IO {
      (new IO).out_string("d\n")
   };
   execute(s: Stack): Stack {
      if s.empty()
      then s
      else {
         s.top().print();
         execute(s.pop());
      }
      fi
   };
};

(*
 * Layout of Stack
 * (Command (Command (Command (Command Nil))))
 *     ^                 ^     ^^^^^^^^^^^
 *    top              bottom     void
 *)
class Stack {
   head: Command;
   tail: Stack;

   set(c: Command, t: Stack): Stack {
      {
         head <- c;
         tail <- t;
         self;
      }
   };
   empty(): Bool {
      isvoid head
   };
   push(c: Command): Stack {
      (new Stack).set(c, self)
   };
   pop(): Stack {
      if empty() then self else tail fi
   };
   -- assure not empty before use the return value of top()
   top(): Command {
      head
   };
};

class Machine {
   stack: Stack <- new Stack;
   cvt: A2I <- new A2I;

   run(): Int {
      let io: IO <- new IO, continue: Bool <- true, cnt: Int <- 0 in {
         while (continue) loop {
            io.out_string(">");
            -- assume all inputs are legal
            let cmd: String <- io.in_string() in {
               if cmd = "+" then {
                  stack <- stack.push(new AddCommand);
               }
               else if cmd = "s" then {
                  stack <- stack.push(new SwapCommand);
               }
               else if cmd = "e" then {
                  stack <- (new EvalCommand).execute(stack);
               }
               else if cmd = "d" then {
                  (new DisplayCommand).execute(stack);
               }
               else if cmd = "x" then {
                  continue <- false;
               }
               else {
                  stack <- stack.push(new IntCommand.set(cvt.a2i(cmd)));
               } fi fi fi fi fi;
               cnt <- cnt + 1;
            };
         } pool;
         cnt;
      }
   };
};

class Main inherits IO {
   m: Machine <- new Machine;

   main(): Object {
      m.run()
   };
};
