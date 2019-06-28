/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import org.junit.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Note}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class NoteTest extends junit.framework.TestCase {
	private final Cache cache = new FieldFormats().cache(Qxsl.NOTE);
	@Test
	public void testValue() {
		assertThat(new Note("RABBIT").value()).isEqualTo("RABBIT");
		assertThat(new Note("GIBIER").value()).isEqualTo("GIBIER");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new Note(text)).hasToString(text);
	}
	@Test
	public void testNote$Format() throws Exception {
		final Note.Format form = new Note.Format();
		final Note note = new Note(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(note))).isEqualTo(note);
		assertThat(cache.field(form.encode(note))).isEqualTo(note);
	}
}
