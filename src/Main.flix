namespace Stack {
  pub opaque type Stack[a] = { stack: List[a] }

  pub def empty(): Stack[a] = Stack({stack = Nil})

  pub def isEmpty(s: Stack[a]): Bool =
    match s {
      case Stack(theStack) => List.isEmpty(theStack.stack)
    }

  pub def push(s: Stack[a], val: a): Stack[a] =
    match s {
      case Stack(theStack) => Stack({stack = val :: theStack.stack})
    }

  pub def pop(s: Stack[a]): (Option[a], Stack[a]) =
    match s {
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
    List.isEmpty(match q {
      case Queue(theQ) => theQ.dequeue
    })

  pub def enq(q: Queue[a], e: a): Queue[a] =
    let theQ = match q {
      case Queue(qq) => qq
    };
    let newQ = e :: theQ.enqueue;
    Queue({enqueue = newQ, dequeue = List.reverse(newQ) | theQ})

  pub def deq(q: Queue[a]): (Option[a], Queue[a]) =
    if (isEmpty(q))
      (None, q)
    else
      let theQ = match q {
        case Queue(qq) => qq
      };
      let newDQ = List.drop(1, theQ.dequeue);
      (List.head(theQ.dequeue), Queue({enqueue = List.reverse(newDQ), dequeue = newDQ}))
}

enum InterceptorContext[config, e] {
  case Context({
    toEnter: Queue.Queue[Interceptor[e]],
    toLeave: Stack.Stack[Interceptor[e]]
    | config
  })
}

type alias CurrentInterceptorContext = InterceptorContext[{log: List[String]}, Impure]

type alias InterceptorFn[e] = CurrentInterceptorContext -> CurrentInterceptorContext & e

pub opaque type Interceptor[e] = {
  enter: Option[InterceptorFn[e]],
  leave: Option[InterceptorFn[e]]
}

def execute(ctx: CurrentInterceptorContext): CurrentInterceptorContext =
  let theCtx = match ctx {
    case Context(c) => c
  };
  let (nextEnterInterceptor, newQueue) = Queue.deq(theCtx.toEnter);
  match nextEnterInterceptor {
    // Enter
    case Some(interceptor) =>
      let newStack = Stack.push(theCtx.toLeave, interceptor);
      match interceptor {
        case Interceptor(i) => match i.enter {
          case Some(fn) => execute(
            fn(Context({toEnter = newQueue, toLeave = newStack | theCtx})))
          case None => execute(Context({toEnter = newQueue, toLeave = newStack | theCtx}))
        }
      }
    // Leave
    case None =>
      let (nextLeaveInterceptor, newStack) = Stack.pop(theCtx.toLeave);
      match nextLeaveInterceptor {
        case None => ctx
        case Some(interceptor) =>
          match interceptor {
            case Interceptor(i) => match i.leave {
              case Some(fn) => execute(
                fn(Context({toLeave = newStack | theCtx}))
              )
              case None => execute(
                Context({toLeave = newStack | theCtx})
              )
            }
          }
      }
  }

def logName(name: String): CurrentInterceptorContext -> CurrentInterceptorContext =
  ctx ->
    let theCtx = match ctx {
      case Context(c) => c
    };
    Context({log = name :: theCtx.log | theCtx})

// The main entry point.
def main(): CurrentInterceptorContext & Impure =
  let enterInt = Interceptor({enter = Some(logName("enter only")), leave = None});
  let enterIntEff = Interceptor({enter = Some(c -> {
      Console.printLine("Test");
      c
  }), leave = None});
  let leaveInt = Interceptor({enter = None, leave = Some(logName("leave only") as Impure)});
  let bothInt = Interceptor({enter = Some(logName("enter then leave") as Impure), leave = Some(logName("leave") as Impure)});
  let someCtx = Context({
    toEnter = Queue.enq(Queue.enq(Queue.enq(Queue.enq(Queue.empty(), leaveInt), enterInt), bothInt), enterIntEff),
    toLeave = Stack.empty(),
    log = Nil
  });
  let finalCtx = execute(someCtx);
  match finalCtx {
    case Context(c) => List.foreach(logLine -> Console.printLine(logLine), c.log)
  };
  finalCtx
