type alias InterceptorFn[e] = () -> () & e

pub enum Interceptor {
  case Enter(InterceptorFn)
}

// This is probably problematic due to no effect annotation
def exec(i: Interceptor): () =
// def exec(i: Interceptor): () & Impure = // has same problem
  match i with {
    case Enter(ifn) => ifn()
  }

// The main entry point.
def main(): () & Impure =
  let enterIntEff = Enter(() -> {
      Console.printLine("Test");
      ()
  });
  let _ = exec(enterIntEff);
  ()
