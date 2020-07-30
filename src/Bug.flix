
namespace Stack {
  pub enum Stack[a] {
    case Empty
    case First(a, Stack[a])
  }

  pub def empty(): Stack[a] = Stack.Empty

  pub def isEmpty(s: Stack[a]): Bool =
    match s with {
      case Empty => true
      case _ => false
    }

  pub def push(s: Stack[a], val: a): Stack[a] = First(val, s)

  pub def pop(s: Stack[a]): (Option[a], Stack[a]) =
    match s with {
      case Empty => (None, Empty)
      case First(val, rest) => (Some(val), rest)
    }
}

namespace Queue {
  pub enum Queue[a] {
    case Empty
    case Next(a, Queue[a])
  }

  pub def empty(): Queue[a] = Queue.Empty

  pub def isEmpty(q: Queue[a]): Bool =
    match q with {
      case Empty => true
      case _ => false
    }

  pub def enq(q: Queue[a], e: a): Queue[a] =
    Next(e, q)

  pub def deq(q: Queue[a]): (Option[a], Queue[a]) =
    match q with {
      case Empty => (None, Empty)
      case Next(e, rest) => (Some(e), rest)
    }
}

enum InterceptorContext[config] {
  case Context({
    toEnter: Queue.Queue[Interceptor],
    toLeave: Stack.Stack[Interceptor],
    log: List[String]
    | config
  })
}

type alias CurrentInterceptorContext = InterceptorContext[{}]

type alias InterceptorFn = CurrentInterceptorContext -> CurrentInterceptorContext

pub enum Interceptor {
  case Enter(InterceptorFn)
  case Leave(InterceptorFn)
  case Both(InterceptorFn, InterceptorFn)
}

def execute(ctx: CurrentInterceptorContext): CurrentInterceptorContext & Impure =
  let theCtx = match ctx with {
    case Context(c) => c
  };
  let (nextEnterInterceptor, newQueue) = Queue.deq(theCtx.toEnter);
  match nextEnterInterceptor with {
    // Enter
    case Some(interceptor) =>
      match interceptor with {
        case Enter(f) =>
          execute(f(Context({toEnter = newQueue | theCtx})))
        case Both(enterFn, _) =>
          let newCtx = match enterFn(Context({toEnter = newQueue | theCtx})) with {
            case Context(c) => c
          };
          let newStack = Stack.push(newCtx.toLeave, interceptor);
          execute(Context({toLeave = newStack | newCtx}))
        case _ =>
          let newStack = Stack.push(theCtx.toLeave, interceptor);
          execute(Context({toEnter = newQueue, toLeave = newStack | theCtx}))
      }
    // Leave
    case None =>
      let (nextLeaveInterceptor, newStack) = Stack.pop(theCtx.toLeave);
      match nextLeaveInterceptor with {
        case None => ctx
        case Some(interceptor) =>
          match interceptor with {
            case Leave(f) => execute(f(Context({toLeave = newStack | theCtx})))
            case Both(_, leaveFn) => execute(leaveFn(Context({toLeave = newStack | theCtx})))
            case _ => execute(Context({toLeave = newStack | theCtx}))
          }
      }
  }

def printNameFun(): CurrentInterceptorContext -> CurrentInterceptorContext =
  ctx ->
    ctx

// The main entry point.
def main(): CurrentInterceptorContext & Impure =
  let enterInt = Enter(printNameFun());
  let leaveInt = Leave(printNameFun());
  let bothInt = Both(printNameFun(), printNameFun());
  let someCtx = Context({
    toEnter = Queue.enq(Queue.enq(Queue.enq(Queue.empty(), leaveInt), enterInt), bothInt),
    toLeave = Stack.empty(),
    log = Nil
  });
  let finalCtx = execute(someCtx);
  finalCtx
