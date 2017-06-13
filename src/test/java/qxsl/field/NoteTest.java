/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import org.junit.Test;
import qxsl.model.Fields;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.NOTE;

/**
 * {@see Note}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class NoteTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(NOTE);
	@Test
	public void testValue() {
		final String str = util.RandText.alnum(100);
		assertThat(new Note(str).value(), is(str));
	}
	@Test
	public void testNote$Format() throws Exception {
		final Note.Format $form = new Note.Format();
		final Note note = new Note(util.RandText.alnum(100));
		assertThat($form.decode($form.encode(note)), is(note));
		assertThat(fields.cache($form.encode(note)), is(note));
	}
}
