/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.draft;

import java.time.ZonedDateTime;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Qxsl;
import qxsl.draft.Sign;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;

/**
 * {@link SignFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/08
 */
public final class SignFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.SIGN);

	@Test
	public void test() throws Exception {
		final var form = new SignFactory();
		final var sign = new Sign(ZonedDateTime.now());
		assertThat(form.decode(form.encode(sign))).isEqualTo(sign);
		assertThat(cache.field(form.encode(sign))).isEqualTo(sign);
	}
}
