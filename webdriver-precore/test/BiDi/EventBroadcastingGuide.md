# WebDriver BiDi Event Broadcasting System

## Overview

This document describes the broadcast channel pattern implementation for WebDriver BiDi event subscription in Haskell. The system allows multiple clients to subscribe to and receive WebSocket events without interfering with command/response flows.

## Architecture

### Core Components

#### EventSubscription
```haskell
data EventSubscription = MkEventSubscription
  { subscriptionId :: Text,
    eventQueue :: TChan (Text, Value),  -- Event delivery queue
    eventFilter :: (Text, Value) -> Bool,  -- Filter predicate  
    isActive :: TVar Bool  -- Subscription state
  }
```

#### EventBroadcaster
```haskell
data EventBroadcaster = MkEventBroadcaster
  { subscriptions :: TVar [EventSubscription],  -- Active subscribers
    nextSubscriptionId :: TVar Word64  -- ID generator
  }
```

#### BiDiMethods Interface
```haskell
data BiDiMethods = MkBiDiMethods
  { -- ... other methods ...
    subscribeToEvents :: ((Text, Value) -> Bool) -> IO EventSubscription,
    unsubscribeFromEvents :: EventSubscription -> IO ()
  }
```

## How It Works

### 1. Message Processing Flow

```
WebSocket Message
       ↓
parseIncomingMessage
       ↓
   ┌─────────┐    ┌──────────────┐
   │Response │    │    Event     │
   │Object   │    │(Text, Value) │
   └─────────┘    └──────────────┘
       ↓                 ↓
  Response Queue    Event Broadcasting
       ↓                 ↓
  Command Waiting   All Subscriptions
```

### 2. Event Broadcasting

When an event is received:
1. Parse message to determine if it's event or response
2. If event: broadcast to all active subscriptions
3. Apply each subscription's filter function
4. Deliver to matching subscription queues

### 3. Subscription Lifecycle

**Subscribe:**
```haskell
subscription <- methods.subscribeToEvents isFileDialogEvent
```

**Monitor:**
```haskell
(eventType, eventValue) <- atomically $ readTChan subscription.eventQueue
```

**Unsubscribe:**
```haskell
methods.unsubscribeFromEvents subscription
```

## Usage Examples

### Basic File Dialog Monitoring

```haskell
fileDialogDemo :: BiDiMethods -> IO ()
fileDialogDemo methods = do
  -- Subscribe to file dialog events
  subscription <- methods.subscribeToEvents isFileDialogEvent
  
  -- Monitor events
  forever $ do
    (eventType, eventValue) <- atomically $ readTChan subscription.eventQueue
    putStrLn $ "File dialog event: " <> T.unpack eventType

isFileDialogEvent :: (Text, Value) -> Bool
isFileDialogEvent (eventType, _) = eventType == "input.fileDialogOpened"
```

### Multiple Event Types

```haskell
-- Subscribe to all input events
subscription <- methods.subscribeToEvents $ \(eventType, _) ->
  "input." `T.isPrefixOf` eventType

-- Subscribe to specific events
subscription <- methods.subscribeToEvents $ \(eventType, _) ->
  eventType `elem` ["input.fileDialogOpened", "browsingContext.load"]
```

### Advanced Pattern Matching

```haskell
-- Filter by event content
subscription <- methods.subscribeToEvents $ \(eventType, eventValue) ->
  case eventType of
    "input.fileDialogOpened" -> 
      -- Could examine eventValue for specific criteria
      True
    "browsingContext.load" ->
      -- Check if specific browsing context
      True
    _ -> False
```

## Implementation Benefits

### 1. **Separation of Concerns**
- Commands/responses remain synchronous
- Events are handled asynchronously
- No interference between flows

### 2. **Multiple Subscribers**
- Each subscription gets its own event queue
- Independent filter predicates
- Concurrent event processing

### 3. **Type Safety**
- Filter functions are typed: `(Text, Value) -> Bool`
- STM ensures thread-safe subscription management
- Clear ownership of resources

### 4. **Flexibility**
- Simple event representation `(Text, Value)`
- Can be upgraded to typed events later
- Composable filter predicates

## Integration with Existing Code

The broadcast system integrates cleanly with existing `BiDiRunner` without changing:
- Command sending (`send`)
- Response receiving (`getNext`) 
- ID generation (`nextId`)

Only adds:
- `subscribeToEvents`
- `unsubscribeFromEvents`

## Future Enhancements

### 1. Typed Events
When `Event` gets `FromJSON` instances:
```haskell
-- Current
subscribeToEvents :: ((Text, Value) -> Bool) -> IO EventSubscription

-- Future  
subscribeToEvents :: (Event -> Bool) -> IO EventSubscription
```

### 2. Event Replay/History
```haskell
data EventBroadcaster = MkEventBroadcaster
  { subscriptions :: TVar [EventSubscription],
    eventHistory :: TVar (Seq Event),  -- Last N events
    maxHistorySize :: Int
  }
```

### 3. Subscription Metadata
```haskell
data EventSubscription = MkEventSubscription
  { subscriptionId :: Text,
    eventQueue :: TChan Event,
    eventFilter :: Event -> Bool,
    isActive :: TVar Bool,
    metadata :: Map Text Text  -- User-defined labels
  }
```

## Error Handling

The system handles several error scenarios:

1. **Inactive Subscriptions**: Events not delivered to inactive subscriptions
2. **Parse Failures**: Malformed events logged but don't crash system  
3. **Queue Overflow**: STM channels handle backpressure naturally
4. **Concurrent Access**: STM provides safe concurrent subscription management

## Performance Considerations

- **Memory**: Each subscription maintains its own queue
- **CPU**: Event filtering happens per subscription
- **Concurrency**: STM scales well with subscription count
- **Backpressure**: Slow subscribers don't block fast ones

## Summary

This broadcast channel pattern provides a robust foundation for WebDriver BiDi event handling. It's:
- **Simple**: Clean API surface
- **Safe**: STM-based concurrency  
- **Scalable**: Multiple independent subscribers
- **Extensible**: Ready for future enhancements

The pattern successfully addresses the original requirement: "log a message when fileDialog is opened" while providing a general solution for any BiDi event monitoring needs.