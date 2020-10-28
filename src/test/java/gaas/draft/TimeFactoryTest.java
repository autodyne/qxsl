/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;

/**
 * {@link TimeFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
public final class TimeFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.TIME);

	@Test
	public void test() throws Exception {
		final var form = new TimeFactory();
		final var time = Time.now();
		assertThat(form.decode(form.encode(time))).isEqualTo(time);
		assertThat(cache.field(form.encode(time))).isEqualTo(time);
	}
}
