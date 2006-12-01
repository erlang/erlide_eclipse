/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.ISourceViewerExtension2;
import org.eclipse.jface.text.source.projection.AnnotationBag;
import org.erlide.ui.editors.util.HTMLPrinter;

public class ErlangAnnotationHover implements IAnnotationHover {

	public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) {
		final List<Annotation> anns = getErlangAnnotationsForLine(sourceViewer, lineNumber);
		if (anns != null) {

			if (anns.size() == 1) {
				// optimization
				final Annotation annotation = anns.get(0);
				final String message = annotation.getText();
				if (message != null && message.trim().length() > 0) {
					return formatSingleMessage(message);
				}
			} else {
				final List<String> messages = new ArrayList<String>();

				final Iterator<Annotation> e = anns.iterator();
				while (e.hasNext()) {
					final Annotation annotation = e.next();
					final String message = annotation.getText();
					if (message != null && message.trim().length() > 0) {
						messages.add(message.trim());
					}
				}

				if (messages.size() == 1) {
					return formatSingleMessage(messages.get(0));
				}

				if (messages.size() > 1) {
					return formatMultipleMessages(messages);
				}
			}
		}
		return null;
	}

	private List<Annotation> getErlangAnnotationsForLine(ISourceViewer viewer, int line) {
		final IAnnotationModel model = getAnnotationModel(viewer);
		if (model == null) {
			return null;
		}

		final IDocument document = viewer.getDocument();
		final List<Annotation> javaAnnotations = new ArrayList<Annotation>();
		final HashMap messagesAtPosition = new HashMap();
		final Iterator iterator = model.getAnnotationIterator();

		while (iterator.hasNext()) {
			Annotation annotation = (Annotation) iterator.next();

			Position position = model.getPosition(annotation);
			if (position == null) {
				continue;
			}

			if (!isRulerLine(position, document, line)) {
				continue;
			}

			if (annotation instanceof AnnotationBag) {
				final AnnotationBag bag = (AnnotationBag) annotation;
				final Iterator e = bag.iterator();
				while (e.hasNext()) {
					annotation = (Annotation) e.next();
					position = model.getPosition(annotation);
					if (position != null
							&& includeAnnotation(annotation, position,
									messagesAtPosition)) {
						javaAnnotations.add(annotation);
					}
				}
				continue;
			}

			if (includeAnnotation(annotation, position, messagesAtPosition)) {
				javaAnnotations.add(annotation);
			}
		}

		return javaAnnotations;
	}

	private boolean includeAnnotation(Annotation annotation, Position position,
			HashMap messagesAtPosition) {
		return true;
	}

	private IAnnotationModel getAnnotationModel(ISourceViewer viewer) {
		if (viewer instanceof ISourceViewerExtension2) {
			final ISourceViewerExtension2 extension = (ISourceViewerExtension2) viewer;
			return extension.getVisualAnnotationModel();
		}
		return viewer.getAnnotationModel();
	}

	private boolean isRulerLine(Position position, IDocument document, int line) {
		if (position.getOffset() > -1 && position.getLength() > -1) {
			try {
				return line == document.getLineOfOffset(position.getOffset());
			} catch (final BadLocationException x) {
			}
		}
		return false;
	}

	/*
	 * Formats a message as HTML text.
	 */
	private String formatSingleMessage(String message) {
		final StringBuffer buffer = new StringBuffer();
		HTMLPrinter.addPageProlog(buffer);
		HTMLPrinter.addParagraph(buffer, HTMLPrinter
				.convertToHTMLContent(message));
		HTMLPrinter.addPageEpilog(buffer);
		return buffer.toString();
	}

	/*
	 * Formats several message as HTML text.
	 */
	private String formatMultipleMessages(List messages) {
		final StringBuffer buffer = new StringBuffer();
		HTMLPrinter.addPageProlog(buffer);
		HTMLPrinter.addParagraph(buffer, HTMLPrinter
				.convertToHTMLContent("Multiple markers at this line"));

		HTMLPrinter.startBulletList(buffer);
		final Iterator e = messages.iterator();
		while (e.hasNext()) {
			HTMLPrinter.addBullet(buffer, HTMLPrinter
					.convertToHTMLContent((String) e.next()));
		}
		HTMLPrinter.endBulletList(buffer);

		HTMLPrinter.addPageEpilog(buffer);
		return buffer.toString();
	}

}
