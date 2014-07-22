; Multithreaded dispatcher kernel for Z80
;
; Copyright (c) 2014, Michael Borisov
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met: 
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer. 
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution. 
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



;===========================
; Data structure definitions
;===========================

;==================
; Dispatcher Object
;==================

; enum DISPATCHER_OBJECT_TYPE {EventNotificationObject = 0,
; EventSynchronizationObject, MutexObject}
;
EVENT_NOTIFICATION_OBJECT	EQU	0
EVENT_SYNCHRONIZATION_OBJECT	EQU	1


; struct DISPATCHER_OBJECT
; {
;     BYTE Type;
;     BYTE SignalState;
;     KTHREAD* WaitListHead;
; }
DISPOBJ_TYPE			EQU	0
DISPOBJ_SIGNALSTATE		EQU	1
DISPOBJ_WAITLISTHEAD		EQU	2
DISPOBJ_SIZE			EQU	4


; struct CONTEXT
; {
;     WORD IY;
;     WORD IX;
;     WORD BC';
;     WORD DE';
;     WORD HL';
;     WORD AF';
;     WORD BC;
;     WORD DE;
;     WORD HL;
;     WORD AF;
;     WORD SP;
; }
CONTEXT_IY			EQU	0
CONTEXT_IX			EQU	2
CONTEXT_BCS			EQU	4
CONTEXT_DES			EQU	6
CONTEXT_HLS			EQU	8
CONTEXT_AFS			EQU	10
CONTEXT_BC			EQU	12
CONTEXT_DE			EQU	14
CONTEXT_HL			EQU	16
CONTEXT_AF			EQU	18
CONTEXT_SP			EQU	20
CONTEXT_SIZE			EQU	22


;
; THREAD_FLAGS bit map
;
THREAD_FLAG_READY		EQU	0
THREAD_FLAG_MINCONTEXT		EQU	1
;
; struct THREAD
; {
;    BYTE flags;
;    BYTE ThreadPriority;
;    KTHREAD* WaitListEntry;
;    KTHREAD* NextThread;
;    CONTEXT context;
; }
;
THREAD_FLAGS			EQU	0
THREAD_PRIORITY			EQU	1
THREAD_WAIT_LIST_ENTRY		EQU	2
THREAD_NEXT_THREAD		EQU	4
THREAD_CONTEXT			EQU	6
THREAD_SIZE			EQU	THREAD_CONTEXT+CONTEXT_SIZE



; Stack for the ISRs
isr_stack	EQU	0


;===========================
; Configuration constants
;===========================


; Stack for the threads

;-------- Thread 0 init data
THREAD_0_STACK_SIZE		EQU	32
;-------- Thread 1 init data
THREAD_1_STACK_SIZE		EQU	32
;-------- Thread 2 init data
THREAD_2_STACK_SIZE		EQU	4




	device	zxspectrum48
	org	8080h-18h

;======================
; OS Kernel entry point
start:	di
	im	2
	ld	hl,0FE00h	;Initialize Interrupt vector table (conservative)
	ld	a,h
	ld	i,a
	ld	de,0FE01h
	ld	bc,0100h
	ld	(hl),080h
	ldir

; Jump to thread 0
	ld	sp,thread_0_stack+THREAD_0_STACK_SIZE-2
	ei
	ret


;======================
; Interrupt service routine
; Full context saving into a global buffer
; (it will be copied to a thread context area later if necessary)
; R register is never saved or restored
; I register is never supposed to be changed by user code
ISR:	ld	(isr_stack-2),sp
	ld	sp,isr_stack-2
	push	af
	push	hl
	push	de
	push	bc
	ex	af,af'
	exx
	push	af
	push	hl
	push	de
	push	bc
	push	ix
	push	iy
; For possible thread preemption, put current thread as the first return candidate
; this is semantically KeRaiseIrql
	ld	a,(current_thread_priority)
	ld	(request_thread),a

; <------------------------ User ISR starts here

	ld	ix,event_1
	call	KeSetSynchrEventFromIsr

; <------------------------ User ISR ends here

; User ISR is finished. Check if a context switch is necessary (KeLowerIrql)
isrend:	ld	hl,current_thread_priority
	ld	a,(request_thread)
	cp	(hl)
	jr	nc,irncs	;No higher-priority threads are ready
; Switch context. Copy saved registers from ISR stack to current thread's context area
	ld	(hl),a		;Set a new current thread
	ld	hl,(current_thread_ptr)
	ld	de,THREAD_CONTEXT
	add	hl,de
	ex	de,hl
	ld	hl,isr_stack-CONTEXT_SIZE
	ld	bc,CONTEXT_SIZE
	ldir
	ld	iy,(current_thread_ptr)
	ld	ix,(request_thread_ptr)
	res	THREAD_FLAG_MINCONTEXT,(iy+THREAD_FLAGS) ;Interrupt preemption, full context restoration
	jp	KiRestoreContext

; Return from ISR without changing thread context
irncs:	pop	iy
	pop	ix
	pop	bc
	pop	de
	pop	hl
	pop	af
	exx
	ex	af,af'
	pop	bc
	pop	de
	pop	hl
	pop	af
	ld	sp,(isr_stack-2)
	ei
	ret

;-------------------------
; KeExitThread
; Ends a thread and removes it from the system thread list
; Yields execution to the next ready thread
; Must be called or jumped to at IRQL=PASSIVE from the thread that exits
; Does not return
KeExitThread:
	di
; Search for the current thread in the system thread list
	ld	de,(current_thread_ptr)
	ld	hl,(thread_list_head)
	or	a
	sbc	hl,de
	jr	nz,kexth0
; A separate path for deletion of the first thread
	ld	ix,(thread_list_head)
	ld	l,(ix+THREAD_NEXT_THREAD)
	ld	h,(ix+THREAD_NEXT_THREAD+1)
	ld	(thread_list_head),hl
	push	de
	pop	iy
	jr	KiSwapThread
kexth0:	add	hl,de
	push	hl
	pop	ix
	ld	l,(ix+THREAD_NEXT_THREAD)
	ld	h,(ix+THREAD_NEXT_THREAD+1)
	or	a
	sbc	hl,de
	jr	nz,kexth0
; The current thread reference is found somewhere in the list, IX=ref_thread, DE=cur_thread
	ld	yh,d
	ld	yl,e
	ld	c,(iy+THREAD_NEXT_THREAD)
	ld	b,(iy+THREAD_NEXT_THREAD+1)
	ld	(ix+THREAD_NEXT_THREAD),c
	ld	(ix+THREAD_NEXT_THREAD+1),b
	jr	KiSwapThread


;=========================
; KeWaitForObject
; <IX - Address of object
; must be called at IRQL=PASSIVE
; uses all registers
KeWaitForObject:
; We potentially need to touch the dispatcher database.
	di
;Check if the object is already signaled
	bit	0,(ix+DISPOBJ_SIGNALSTATE)
	jr	z,WFS_0
; If it is not a synchronization event, return
	ld	a,(ix+DISPOBJ_TYPE)
	cp	EVENT_SYNCHRONIZATION_OBJECT
	jr	nz,eiret0
; Unsignal it before returning
	res	0,(ix+DISPOBJ_SIGNALSTATE)
eiret0:	ei
	ret
; Setup of the waiting thread
WFS_0:	ld	iy,(current_thread_ptr)
; Insert the thread into the wait list for this object
	ld	l,(ix+DISPOBJ_WAITLISTHEAD)
	ld	h,(ix+DISPOBJ_WAITLISTHEAD)
	ld	a,yl
	ld	(ix+DISPOBJ_WAITLISTHEAD),a
	ld	a,yh
	ld	(ix+DISPOBJ_WAITLISTHEAD+1),a
	ld	(iy+THREAD_WAIT_LIST_ENTRY),l
	ld	(iy+THREAD_WAIT_LIST_ENTRY+1),h

;-----------------------
; KiSwapThread
; Picks a lower priority thread to run when the current one gets blocked.
; Saves a minimal context of the current thread (SP, and PC on the thread's stack)
; Restores context of the ready thread
; Must be called at IRQL=DISPATCH_LEVEL
; <IY - Address of the current thread
KiSwapThread:
; Search for a thread with a lower priority to run
; (no threads with higher priority are guaranteed to be ready,
; otherwise the current thread would have been preeempted)
	push	iy
	pop	ix
SWT_0:	ld	e,(ix+THREAD_NEXT_THREAD)
	ld	d,(ix+THREAD_NEXT_THREAD+1)
	ld	xh,d
	ld	xl,e
	bit	THREAD_FLAG_READY,(ix+THREAD_FLAGS)
	jr	z,SWT_0
; A thread is found. Make it current
	ld	a,(ix+THREAD_PRIORITY)
	ld	(current_thread_priority),a
	res	THREAD_FLAG_READY,(iy+THREAD_FLAGS)

;======================
; KiSwitchContext
;  Switch thread context
; Saves a minimal context of the current thread (SP, and PC on the thread's stack)
; Restores context of the target thread
; Must be called at IRQL=DISPATCH_LEVEL
; Returns when the current thread gets a CPU time slice again, at PASSIVE_LEVEL
; <IY - address of the current thread
; <IX - address of the target thread
KiSwitchContext:
	set	THREAD_FLAG_MINCONTEXT,(iy+THREAD_FLAGS) ;Minimal context restoration after waiting
; Minimal context saving
	ld	hl,0
	add	hl,sp
	ld	(iy+THREAD_CONTEXT+CONTEXT_SP),l
	ld	(iy+THREAD_CONTEXT+CONTEXT_SP+1),h

;======================
; KiRestoreContext
; Restores context of the target thread
; Must be jumped to at IRQL>=DISPATCH_LEVEL
; Does not return
; <IX - address of the target thread
KiRestoreContext:
	ld	(current_thread_ptr),ix
; Context restoration: restore minimal or full context depending on thread flags
	bit	THREAD_FLAG_MINCONTEXT,(ix+THREAD_FLAGS)
	jr	nz,kscmin
; Full context restoration
	ld	de,THREAD_CONTEXT
	add	ix,de
	ld	sp,ix
	pop	iy
	pop	ix
	pop	bc
	pop	de
	pop	hl
	pop	af
	ex	af,af'
	exx
	pop	bc
	pop	de
	pop     hl
	ld	(rgsav),hl
	pop	af
	pop	hl
	ld	sp,hl
	ld	hl,(rgsav)
	ei
	ret
; Minimal context restoration
kscmin:	ld	l,(ix+THREAD_CONTEXT+CONTEXT_SP)
	ld	h,(ix+THREAD_CONTEXT+CONTEXT_SP+1)
	ld	sp,hl
eiret:	ei
	ret

;--------------------------
; KeResetEvent
; <IX - address of the event
; can be called at any IRQL
; uses none
KeResetEvent:
	res	0,(ix+DISPOBJ_SIGNALSTATE)
	ret

;--------------------------
; KeSetNotifEvent
; Must be called at IRQL=PASSIVE
; <IX - address of event
; uses all registers
KeSetNotifEvent:
	ld	a,(current_thread_priority)
	di
	ld	(request_thread),a
	call	KeSetNotifEventFromIsr
; Check if we need to switch to a higher priority thread right now
	jr	KiLowerIrqlFromDispatch

;---------------------------
; KeSetNotifEventFromIsr
; common code for notification event setting
; must be called at IRQL>=DISPATCH_LEVEL
; <IX - address of event
; uses IY, DE, HL, A
KeSetNotifEventFromIsr:
	set	0,(ix+DISPOBJ_SIGNALSTATE)
; Unwait all threads that may be waiting for this event
	ld	e,(ix+DISPOBJ_WAITLISTHEAD)
	ld	d,(ix+DISPOBJ_WAITLISTHEAD+1)
ksne0:	ld	a,d
	or	e
	jr	z,ksne1		;Exit loop if no (more) waiters
	ld	yh,d
	ld	yl,e
	call	KiUnwaitThread
; Move on to the next thread in the wait list
	ld	e,(iy+THREAD_WAIT_LIST_ENTRY)
	ld	d,(iy+THREAD_WAIT_LIST_ENTRY+1)
	jr	ksne0
; Clear the object's wait list. DE guaranteed to be 0000
ksne1:	ld	(ix+DISPOBJ_WAITLISTHEAD),e
	ld	(ix+DISPOBJ_WAITLISTHEAD+1),d
	ret


;---------------------------
; KiLowerIrqlFromDispatch
; After satisfying waits, check if any of the woken
; threads can preempt the current thread
; Must be called at IRQL=DISPATCH_LEVEL
; Returns at IRQL=passive, possibly after switching to another thread and back
; uses all registers
KiLowerIrqlFromDispatch:
	ld	hl,current_thread_priority
	ld	a,(request_thread)
	cp	(hl)
	jr	nc,eiret	;No higher-priority threads are waiting
; The requesting thread preempts us. Switch the context to it
	ld	(hl),a
	ld	iy,(current_thread_ptr)
	ld	ix,(request_thread_ptr)
	jp	KiSwitchContext ;Non-interrupt preemption, min context restoration

;---------------------------
; KiUnwaitThread
; Must be called at IRQL>=DISPATCH_LEVEL
; <IY - address of thread
; uses A, HL
KiUnwaitThread:
	; Set the thread's state to Ready
	set	THREAD_FLAG_READY,(iy+THREAD_FLAGS)
	; Check if the thread's priority is higher than of any other requesters
	ld	a,(iy+THREAD_PRIORITY)
	ld	hl,request_thread
	cp	(hl)
	ret	nc
	ld	(hl),a			;the new thread has a higher priority
	ld	(request_thread_ptr),iy
	ret

;--------------------------
; KeSetSynchrEvent
; Must be called at IRQL=PASSIVE
; <IX - address of event
; uses all registers
KeSetSynchrEvent:
	di
	set	0,(ix+DISPOBJ_SIGNALSTATE)
; Check if anybody was waiting for this event
	ld	e,(ix+DISPOBJ_WAITLISTHEAD)
	ld	d,(ix+DISPOBJ_WAITLISTHEAD+1)
	ld	a,d
	or	e
	jp	z,eiret	;If no waiters, quick return
	ld	a,(current_thread_priority)
	ld	(request_thread),a
	call	KiRemoveUnwaitSingleThread
; Check if we need to switch to that thread right now
	jr	KiLowerIrqlFromDispatch


;---------------------------
; KeSetSynchrEventFromIsr
; Must be called at IRQL>=DISPATCH_LEVEL
; <IX - address of event
; uses IY, DE, HL, A
KeSetSynchrEventFromIsr:
	set	0,(ix+DISPOBJ_SIGNALSTATE)
; Check if anybody was waiting for this event
	ld	e,(ix+DISPOBJ_WAITLISTHEAD)
	ld	d,(ix+DISPOBJ_WAITLISTHEAD+1)
	ld	a,d
	or	e
	ret	z	;If no waiters, quick return
;----------------------------
; KiRemoweUnwaitSingleThread
; Removes a single thread from the object's waiting list
; and unwaits it. Also resets the objet's signal state
; Must be called at IRQL>=DISPATCH_LEVEL
; <IX - address of dispatcher object
; <DE - address of a thread waiting on this object
; >IY - address of thread (same as DE)
; uses A, HL
KiRemoveUnwaitSingleThread:
	res	0,(ix+DISPOBJ_SIGNALSTATE)
	ld	yh,d
	ld	yl,e
; Remove the thread from the event's wait list
	ld	e,(iy+THREAD_WAIT_LIST_ENTRY)
	ld	d,(iy+THREAD_WAIT_LIST_ENTRY+1)
	ld	(ix+DISPOBJ_WAITLISTHEAD),e
	ld	(ix+DISPOBJ_WAITLISTHEAD+1),d
	jr	KiUnwaitThread


;-------------------------
; KeStartThread
; Starts a new thread and schedules it for execution
; New threads always start in Ready state
; Can preempt current thread in favor of the new thread
;  if the new thread's priority is higher
; Must be called at IRQL=PASSIVE
; <IY - address of the new thread
; uses all registers
KeStartThread:
	di
	ld	a,(current_thread_priority)
	ld	(request_thread),a
	call	KiStartThread
	jr	KiLowerIrqlFromDispatch

;-------------------------
; KiStartThread
; Starts a new thread by inserting it in the system thread list sorted by priority
; Must be called at IRQL=DISPATCH_LEVEL
; <IY - address of the new thread
; uses HL,DE,BC,A,IX
KiStartThread:
	ld	ix,(thread_list_head)
	ld	a,(iy+THREAD_PRIORITY)
	cp	(ix+THREAD_PRIORITY)
	jr	nc,kisth0
; A separate path for insertion in the list head
	ld	b,xh
	ld	c,xl
	ld	(iy+THREAD_NEXT_THREAD),c
	ld	(iy+THREAD_NEXT_THREAD+1),b
	ld	(thread_list_head),ix ;The new list head
	jr	kisth1
; Move on to the next thread
kisth0:	ld	c,(ix+THREAD_NEXT_THREAD)
	ld	b,(ix+THREAD_NEXT_THREAD+1)
; Save the thread pointer for the next iteration
	ld	d,xh
	ld	e,xl
	ld	xh,b
	ld	xl,c
	cp	(ix+THREAD_PRIORITY)
	jr	nc,kisth0
; A place for insertion is found
	ld	b,xh
	ld	c,xl
	ld	(iy+THREAD_NEXT_THREAD),c
	ld	(iy+THREAD_NEXT_THREAD+1),b
	ld	xh,d
	ld	xl,e
	ld	b,yh
	ld	c,yl
	ld	(ix+THREAD_NEXT_THREAD),c
	ld	(ix+THREAD_NEXT_THREAD+1),b
; After insertion, compete for the highest priority
kisth1:	ld	hl,request_thread
	cp	(hl)
	ret	nc
; We are the highest requester
	ld	(hl),a
	ld	(request_thread_ptr),iy
	ret






;====================================
;   Example threads
;====================================
thread_0_entry:
	ld	iy,thread_1
	call	KeStartThread
thr00:	ld	ix,event_0
	call	KeWaitForObject
	ld	ix,event_0
	call	KeResetEvent
	jr	thr00

thread_1_entry:
	ld	ix,event_1
	call	KeWaitForObject
	ld	ix,event_0
	call	KeSetSynchrEvent
	jp	KeExitThread

thread_2_entry:
	halt
	jr	thread_2_entry


;====================================
;   Data segment
;====================================

current_thread_priority:	db	0
current_thread_ptr: dw	thread_0
thread_list_head: dw thread_0

;-- Thread 0 (priority 0 = highest)
thread_0:
	db	3	;Flags: Ready, min-context
	db	0	;Thread priority (lower=higher)
	dw	0	;WaitListEntry
	dw	thread_idle ;NextThread
	ds	CONTEXT_SIZE-2
	dw	thread_0_stack+THREAD_0_STACK_SIZE-2
;-- Thread 1 (priority 1)
thread_1:
	db	3	;Flags: Ready, min-context
	db	1	;Thread priority (lower=higher)
	dw	0	;WaitListEntry
	dw	0	;NextThread
	ds	CONTEXT_SIZE-2
	dw	thread_1_stack+THREAD_1_STACK_SIZE-2
;-- System Idle Thread (priority 255, lowest, must not wait or exit)
thread_idle:
	db	3	;Flags: Ready, min-context
	db	2	;Thread priority (lower=higher)
	dw	0	;WaitListEntry
	dw	0	;NextThread - has no meaning
	ds	CONTEXT_SIZE-2
	dw	thread_2_stack+THREAD_2_STACK_SIZE-2

thread_0_stack:	ds	THREAD_0_STACK_SIZE-2
	dw	thread_0_entry

thread_1_stack:	ds	THREAD_1_STACK_SIZE-2
	dw	thread_1_entry

thread_2_stack:	ds	THREAD_2_STACK_SIZE-2
	dw	thread_2_entry


event_0:
	db	EVENT_NOTIFICATION_OBJECT
	db	0
	dw	0

event_1:
	db	EVENT_SYNCHRONIZATION_OBJECT
	db	0
	dw	0



last_addr_sav:


request_thread:	ds	1
request_thread_ptr: ds	2

rgsav:	ds	2


	savebin "mzkernel.bin",start,last_addr_sav-start

