/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.field.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.BaseFormat.*;

/**
 * {@link Fields}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/25
 *
 */
public final class FieldsTest extends junit.framework.TestCase {
	private final Fields fields = new Fields();
	@Test
	public void testIterator() {
		assertThat(fields.iterator(), is(notNullValue()));
		assertThat(fields.iterator().hasNext(), is(true));
	}
	@Test
	public void testGetFormat() {
		assertThat(fields.getFormat(BAND), is(instanceOf(Band.Format.class)));
		assertThat(fields.getFormat(CALL), is(instanceOf(Call.Format.class)));
		assertThat(fields.getFormat(CITY), is(instanceOf(City.Format.class)));
		assertThat(fields.getFormat(CODE), is(instanceOf(Code.Format.class)));
		assertThat(fields.getFormat(MODE), is(instanceOf(Mode.Format.class)));
		assertThat(fields.getFormat(NAME), is(instanceOf(Name.Format.class)));
		assertThat(fields.getFormat(NOTE), is(instanceOf(Note.Format.class)));
		assertThat(fields.getFormat(RSTQ), is(instanceOf(RSTQ.Format.class)));
		assertThat(fields.getFormat(TIME), is(instanceOf(Time.Format.class)));
		assertThat(fields.getFormat(WATT), is(instanceOf(Watt.Format.class)));
	}
}
