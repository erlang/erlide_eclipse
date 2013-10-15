package nl.kii.util

import java.io.Closeable

import static extension nl.kii.util.OptExtensions.*

class CloseableExtensions {
	
	// CLOSING
	
	/** Perform an operation on a closable, and close it when finished */
	def static <I extends Closeable> using(I closable, (I)=>void fn) {
		try {
			fn.apply(closable)
		} finally {
			closable.close
		}
	}

	/** Perform an operation on a closable, and close it when finished */
	def static <I extends Closeable, T> using(I closable, (I)=>T fn) {
		try {
			fn.apply(closable)
		} finally {
			closable.close
		}
	}

	def static <I extends Closeable, T> Opt<T> attemptUsing(I closable, (I)=>T fn) {
		try {
			using(closable, fn).option
		} catch(Throwable e) {
			error(e)
		}
	}
	
}