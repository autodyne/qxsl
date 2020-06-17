/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.field;

import qxsl.extra.field.*;

import org.junit.jupiter.api.Test;

/**
 * {@link FieldFormats}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class FieldFormatsTest extends org.assertj.core.api.Assertions {
	private final FieldFormats fields = new FieldFormats();

	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}

	@Test
	public void testGetFormat() {
		assertThat(fields.forName(Qxsl.BAND)).isInstanceOf(Band.Format.class);
		assertThat(fields.forName(Qxsl.CALL)).isInstanceOf(Call.Format.class);
		assertThat(fields.forName(Qxsl.CITY)).isInstanceOf(City.Format.class);
		assertThat(fields.forName(Qxsl.CODE)).isInstanceOf(Code.Format.class);
		assertThat(fields.forName(Qxsl.BAND)).isInstanceOf(Band.Format.class);
		assertThat(fields.forName(Qxsl.MODE)).isInstanceOf(Mode.Format.class);
		assertThat(fields.forName(Qxsl.NAME)).isInstanceOf(Name.Format.class);
		assertThat(fields.forName(Qxsl.NOTE)).isInstanceOf(Note.Format.class);
		assertThat(fields.forName(Qxsl.RSTQ)).isInstanceOf(RSTQ.Format.class);
		assertThat(fields.forName(Qxsl.TIME)).isInstanceOf(Time.Format.class);
		assertThat(fields.forName(Qxsl.WATT)).isInstanceOf(Watt.Format.class);
	}
}
