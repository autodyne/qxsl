/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Qxsl;
import qxsl.draft.Text;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link TextFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/06
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class TextFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.TEXT);

	@Test
	public void test(@RandomString String text) throws Exception {
		final var form = new TextFactory();
		final var note = new Text(text);
		assertThat(form.decode(form.encode(note))).isEqualTo(note);
		assertThat(cache.field(form.encode(note))).isEqualTo(note);
	}
}
