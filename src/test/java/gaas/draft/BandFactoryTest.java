/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomNumberParameterExtension;
import qxsl.junit.RandomNumberParameterExtension.RandomNumber;

/**
 * {@link BandFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class BandFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.BAND);

	@Test
	public void test(@RandomNumber int num) throws Exception {
		final var form = new BandFactory();
		final var band = new Band(num);
		assertThat(form.decode(form.encode(band))).isEqualTo(band);
		assertThat(cache.field(form.encode(band))).isEqualTo(band);
	}
}
