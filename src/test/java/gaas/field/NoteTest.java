/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.field;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Note}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class NoteTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.NOTE);

	@Test
	public void testValue() {
		assertThat(new Note("RABBIT").value()).isEqualTo("RABBIT");
		assertThat(new Note("GIBIER").value()).isEqualTo("GIBIER");
	}

	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Note(text)).hasToString(text);
	}

	@Test
	public void testNote$Format(@RandomString String text) throws Exception {
		final var form = new Note.Factory();
		final var note = new Note(text);
		assertThat(form.decode(form.encode(note))).isEqualTo(note);
		assertThat(cache.field(form.encode(note))).isEqualTo(note);
	}
}
