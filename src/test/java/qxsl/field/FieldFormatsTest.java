/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.field;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.extra.field.*;

/**
 * {@link FieldManager}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class FieldFormatsTest extends Assertions {
	private final FieldManager fields = new FieldManager();

	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}

	@Test
	public void testGetFormat() {
		assertThat(fields.forName(Qxsl.BAND)).isInstanceOf(Band.Factory.class);
		assertThat(fields.forName(Qxsl.CALL)).isInstanceOf(Call.Factory.class);
		assertThat(fields.forName(Qxsl.CITY)).isInstanceOf(City.Factory.class);
		assertThat(fields.forName(Qxsl.CODE)).isInstanceOf(Code.Factory.class);
		assertThat(fields.forName(Qxsl.BAND)).isInstanceOf(Band.Factory.class);
		assertThat(fields.forName(Qxsl.MODE)).isInstanceOf(Mode.Factory.class);
		assertThat(fields.forName(Qxsl.NAME)).isInstanceOf(Name.Factory.class);
		assertThat(fields.forName(Qxsl.NOTE)).isInstanceOf(Note.Factory.class);
		assertThat(fields.forName(Qxsl.RSTQ)).isInstanceOf(RSTQ.Factory.class);
		assertThat(fields.forName(Qxsl.TIME)).isInstanceOf(Time.Factory.class);
		assertThat(fields.forName(Qxsl.WATT)).isInstanceOf(Watt.Factory.class);
	}
}
