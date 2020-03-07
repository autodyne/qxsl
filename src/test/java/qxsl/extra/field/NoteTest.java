/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.field;

import org.junit.jupiter.api.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Note}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class NoteTest extends test.RandTest {
	private final Cache cache = new FieldFormats().cache(Qxsl.NOTE);
	@Test
	public void testValue() {
		assertThat(new Note("RABBIT").value()).isEqualTo("RABBIT");
		assertThat(new Note("GIBIER").value()).isEqualTo("GIBIER");
	}
	@Test
	public void testToString() {
		final String text = alnum(100);
		assertThat(new Note(text)).hasToString(text);
	}
	@Test
	public void testNote$Format() throws Exception {
		final Note.Format form = new Note.Format();
		final Note note = new Note(alnum(100));
		assertThat(form.decode(form.encode(note))).isEqualTo(note);
		assertThat(cache.field(form.encode(note))).isEqualTo(note);
	}
}
