namespace Stack {
  pub opaque type Stack[a] = { stack: List[a] }

  pub def empty(): Stack[a] = Stack({stack = Nil})

  pub def isEmpty(s: Stack[a]): Bool =
    match s with {
      case Stack(theStack) => List.isEmpty(theStack.stack)
    }

  pub def push(s: Stack[a], val: a): Stack[a] =
    match s with {
      case Stack(theStack) => Stack({stack = val :: theStack.stack})
    }

  pub def pop(s: Stack[a]): (Option[a], Stack[a]) =
    match s with {
      case Stack(theStack) => (
        List.head(theStack.stack),
        Stack({stack = List.drop(1, theStack.stack)})
      )
    }
}

namespace Queue {
  pub opaque type Queue[a] = {
    enqueue: List[a],
    dequeue: List[a]
  }

  pub def empty(): Queue[a] =
    Queue({enqueue = Nil, dequeue = Nil})

  pub def isEmpty(q: Queue[a]): Bool =
    List.isEmpty(match q with {
      case Queue(theQ) => theQ.dequeue
    })

  pub def enq(q: Queue[a], e: a): Queue[a] =
    let theQ = match q with {
      case Queue(qq) => qq
    };
    let newQ = e :: theQ.enqueue;
    Queue({enqueue = newQ, dequeue = List.reverse(newQ) | theQ})

  pub def deq(q: Queue[a]): (Option[a], Queue[a]) =
    if (isEmpty(q))
      (None, q)
    else
      let theQ = match q with {
        case Queue(qq) => qq
      };
      let newDQ = List.drop(1, theQ.dequeue);
      (List.head(theQ.dequeue), Queue({enqueue = List.reverse(newDQ), dequeue = newDQ}))
}

enum InterceptorContext[config] {
  case Context({
    toEnter: Queue.Queue[Interceptor],
    toLeave: Stack.Stack[Interceptor]
    | config
  })
}

type alias CurrentInterceptorContext = InterceptorContext[{log: List[String]}]

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

def logName(name: String): CurrentInterceptorContext -> CurrentInterceptorContext =
  ctx ->
    let theCtx = match ctx with {
      case Context(c) => c
    };
    Context({log = name :: theCtx.log | theCtx})

// The main entry point.
def main(): CurrentInterceptorContext & Impure =
  let enterInt = Enter(logName("enter only"));
  let leaveInt = Leave(logName("leave only"));
  let bothInt = Both(logName("enter then leave"), logName("leave"));
  let someCtx = Context({
    toEnter = Queue.enq(Queue.enq(Queue.enq(Queue.empty(), leaveInt), enterInt), bothInt),
    toLeave = Stack.empty(),
    log = Nil
  });
  let finalCtx = execute(someCtx);
  match finalCtx with {
    case Context(c) => List.foreach(logLine -> Console.printLine(logLine), c.log)
  };
  finalCtx
