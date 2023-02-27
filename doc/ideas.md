# OS Thoughts

Kernel deals with as little as possible:

## Interrupt handling
 * All interrupts just disable the interrupt (in the case of level-sensitive interrupts) and trigger/increment a semaphore
 * All faults are handled in kernel mode (may end up blocking the faulted thread, e.g. page fault requires loading from persistent storage)

## Memory management
 * address translator management (page table lookups/modification)
 * page lifetime management
 * frame management (overflow handling, zeroing)
    * 2048 physical frames (8MB)
    * Fixed 4096 bytes (1 page): array of 16 bits of metadata about each physical frame:
        * 1b: presence flag: when 0, frame is unused.  The frame number in this case forms a linked list to more free frames.
        * 1b: pinned flag: Indicates that this frame must not be evicted.
        * 1b: accessed flag: Indicates that this frame was accessed during the previous period, and thus shouldn't be evicted if possible.
        * 1b: volatile flag: Indicates that this frame shouldn't be persisted.
        * 12b: frame number containing additional metadata about this frame - refcount/list, content ID, etc.
    * Fixed 32 bytes: Array of 16 bits of metadata about each of 16 ASN numbers:
        * 4b: reserved
        * 12b: frame number containing page table and other process info
        * When unused, all bits must be 0

## Event/semaphore management
 * probably want the ability to notify one semaphore, then wait on another, while donating the rest of the task's timeframe to the first waiter of the first semaphore, when possible

## Thread management
 * creation of new threads
 * scheduling of runnable threads
    * Adaptive lottery based selection
        * Priority is composed of 3 parts: initial, incremental, and current.
            * "current" priority is the weight for that task in the lottery.
            * When a task becomes runnable, the current priority is set to the initial priority.
            * Every time the scheduler doesn't pick a task, it adds the incremental priority to the current priority.
    * Scheduling is triggered on frame interrupt (schedules all contexts) or whenever an active task exits or sleeps (schedules single context)

## Process management
 * Arbiter process sets up shared memory queues with every process for message passing
 * Shared memory can be set up directly between processes through the arbiter