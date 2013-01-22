/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.internal.information;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractReusableInformationControlCreator;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInputChangedListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.SimpleSelectionProvider;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;
import org.erlide.ui.util.eclipse.text.BrowserInformationControlInput;
import org.erlide.ui.util.eclipse.text.BrowserInput;
import org.erlide.ui.views.EdocView;
import org.erlide.utils.ErlLogger;

public final class PresenterControlCreator extends
        AbstractReusableInformationControlCreator {

    /**
     * Action to go back to the previous input in the hover control.
     */
    private static final class BackAction extends Action {
        private final BrowserInformationControl fInfoControl;

        public BackAction(final BrowserInformationControl infoControl) {
            fInfoControl = infoControl;
            setText("Previous");
            final ISharedImages images = PlatformUI.getWorkbench()
                    .getSharedImages();
            setImageDescriptor(images
                    .getImageDescriptor(ISharedImages.IMG_TOOL_BACK));
            setDisabledImageDescriptor(images
                    .getImageDescriptor(ISharedImages.IMG_TOOL_BACK_DISABLED));

            update();
        }

        @Override
        public void run() {
            final BrowserInformationControlInput previous = (BrowserInformationControlInput) fInfoControl
                    .getInput().getPrevious();
            if (previous != null) {
                fInfoControl.setInput(previous);
            }
        }

        public void update() {
            final BrowserInformationControlInput current = fInfoControl
                    .getInput();

            if (current != null && current.getPrevious() != null) {
                final BrowserInput previous = current.getPrevious();
                setToolTipText(String.format("Go back to %s",
                        previous.getInputName()));
                setEnabled(true);
            } else {
                setToolTipText("");
                setEnabled(false);
            }
        }
    }

    /**
     * Action to go forward to the next input in the hover control.
     */
    private static final class ForwardAction extends Action {
        private final BrowserInformationControl fInfoControl;

        public ForwardAction(final BrowserInformationControl infoControl) {
            fInfoControl = infoControl;
            setText("Next");
            final ISharedImages images = PlatformUI.getWorkbench()
                    .getSharedImages();
            setImageDescriptor(images
                    .getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD));
            setDisabledImageDescriptor(images
                    .getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD_DISABLED));

            update();
        }

        @Override
        public void run() {
            final BrowserInformationControlInput next = (BrowserInformationControlInput) fInfoControl
                    .getInput().getNext();
            if (next != null) {
                fInfoControl.setInput(next);
            }
        }

        public void update() {
            final BrowserInformationControlInput current = fInfoControl
                    .getInput();

            if (current != null && current.getNext() != null) {
                setToolTipText(String.format("Go to next %s", current.getNext()
                        .getInputName()));
                setEnabled(true);
            } else {
                setToolTipText("");
                setEnabled(false);
            }
        }
    }

    /**
     * Action that shows the current hover contents in the Edoc view.
     */
    private static final class ShowInEdocViewAction extends Action {
        private final BrowserInformationControl fInfoControl;

        public ShowInEdocViewAction(final BrowserInformationControl infoControl) {
            fInfoControl = infoControl;
            setText("Show in eDoc view");
            setImageDescriptor(ErlideImage.OBJS_EDOCTAG.getDescriptor());
        }

        /*
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            final BrowserInformationControlInput input = fInfoControl
                    .getInput();
            fInfoControl.notifyDelayedInputChange(null);
            fInfoControl.dispose();
            try {
                final EdocView view = (EdocView) ErlideUIPlugin.getActivePage()
                        .showView(EdocView.ID);
                // TODO view.setInput(infoInput);
                view.setText(input.getHtml());
            } catch (final PartInitException e) {
                ErlLogger.error(e);
            }
        }
    }

    final ErlangEditor editor;

    public PresenterControlCreator(final ErlangEditor editor) {
        this.editor = editor;
    }

    @Override
    protected IInformationControl doCreateInformationControl(final Shell parent) {
        if (BrowserInformationControl.isAvailable(parent)) {
            final ToolBarManager tbm = new ToolBarManager(SWT.FLAT);

            final String font = JFaceResources.DIALOG_FONT;
            final BrowserInformationControl control = new BrowserInformationControl(
                    parent, font, tbm);

            final PresenterControlCreator.BackAction backAction = new PresenterControlCreator.BackAction(
                    control);
            backAction.setEnabled(false);
            tbm.add(backAction);
            final PresenterControlCreator.ForwardAction forwardAction = new PresenterControlCreator.ForwardAction(
                    control);
            tbm.add(forwardAction);
            forwardAction.setEnabled(false);

            final PresenterControlCreator.ShowInEdocViewAction showInEdocViewAction = new PresenterControlCreator.ShowInEdocViewAction(
                    control);
            tbm.add(showInEdocViewAction);
            final OpenDeclarationAction openDeclarationAction = new OpenDeclarationAction(
                    control, editor);
            tbm.add(openDeclarationAction);

            final SimpleSelectionProvider selectionProvider = new SimpleSelectionProvider();
            final OpenEdocInExternalBrowserAction openEdocInExternalBrowserAction = new OpenEdocInExternalBrowserAction(
                    editor.getSite(), null);
            openEdocInExternalBrowserAction
                    .setSpecialSelectionProvider(selectionProvider);
            selectionProvider
                    .addSelectionChangedListener(openEdocInExternalBrowserAction);
            final ImageDescriptor descriptor = ErlideImage.OBJS_EXTERNALBROWSER
                    .getDescriptor();
            openEdocInExternalBrowserAction.setImageDescriptor(descriptor);
            openEdocInExternalBrowserAction
                    .setDisabledImageDescriptor(descriptor);
            selectionProvider.setSelection(new StructuredSelection());
            tbm.add(openEdocInExternalBrowserAction);

            // OpenExternalBrowserAction openExternalJavadocAction = new
            // OpenExternalBrowserAction(
            // parent.getDisplay(), selectionProvider);
            // selectionProvider
            // .addSelectionChangedListener(openExternalJavadocAction);
            // selectionProvider.setSelection(new
            // StructuredSelection());
            // tbm.add(openExternalJavadocAction);

            final IInputChangedListener inputChangeListener = new IInputChangedListener() {
                @Override
                public void inputChanged(final Object newInput) {
                    backAction.update();
                    forwardAction.update();
                    if (newInput == null) {
                        selectionProvider
                                .setSelection(new StructuredSelection());
                    } else if (newInput instanceof BrowserInformationControlInput) {
                        final BrowserInformationControlInput input = (BrowserInformationControlInput) newInput;
                        final Object inputElement = input.getInputElement();
                        selectionProvider.setSelection(new StructuredSelection(
                                inputElement));
                        final boolean hasInputElement = inputElement != null;
                        showInEdocViewAction.setEnabled(hasInputElement);
                        openDeclarationAction.setEnabled(hasInputElement);
                        openEdocInExternalBrowserAction.setInput(newInput);
                        openEdocInExternalBrowserAction
                                .setEnabled(hasInputElement);
                    }
                }
            };
            control.addInputChangeListener(inputChangeListener);

            tbm.update(true);

            control.addLocationListener(new HandleEdocLinksLocationListener(
                    control));

            return control;
        } else {
            return new DefaultInformationControl(parent,
                    EditorsUI.getTooltipAffordanceString(),
                    new ErlInformationPresenter(true));
        }
    }
}
