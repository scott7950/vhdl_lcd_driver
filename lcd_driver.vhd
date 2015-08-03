library ieee;
use ieee.std_logic_1164.all;

entity lcd_driver is
port (
    clk       : in  std_logic;
    rst       : in  std_logic;
    --refresh   : in  std_logic;
    data0     : in  std_logic_vector(31 downto 0);
    data1     : in  std_logic_vector(31 downto 0);
    data2     : in  std_logic_vector(31 downto 0);
    data3     : in  std_logic_vector(31 downto 0);
    rs        : out std_logic;
    rw        : out std_logic;
    e         : out std_logic;
    db_in     : in  std_logic_vector(7 downto 0);
    db_out    : out std_logic_vector(7 downto 0);
    db_out_en : out std_logic
);
end entity lcd_driver;

architecture behavioral of lcd_driver is
type one_transaction_status is (IDLE, RS_RW1_STATE, E1_STATE, DB_STATE, E2_STATE, RS_RW2_STATE, DONE);
type lcd_seq_status is (LCD_INIT, FUNC_SET, DISP_ON, MODE_SET, CLEAR_SCR, REFRESH_STATE, RET_HOME, ADDR0_SET, WRITE_DATA0, ADDR1_SET, WRITE_DATA1, CHECK_BUSY_FLAG);
type cmd_status is (CLR_SCR, RT_HOME, MODE, DISP, FUNC, DDRAM_ADDR, BUSY_F, WRITE_DATA, NONE);
signal lcd_int_state     : one_transaction_status;
signal lcd_seq_state     : lcd_seq_status;
signal lcd_seq_state_nxt : lcd_seq_status;
signal rs_pre            : std_logic;
signal rw_pre            : std_logic;
signal db_pre            : std_logic_vector(31 downto 0);
signal one_trans_req     : std_logic;
signal lcd_int_cnt       : integer;
signal data              : std_logic_vector(7 downto 0);
signal read_write        : std_logic;
signal busy_flag         : std_logic;
signal cmd_type          : cmd_status;
signal data_no           : integer;
signal lcd_seq_cnt       : integer;
signal write_data_trans  : std_logic;
signal sel               : integer;
signal sel_pre           : integer;
signal dout_tmp          : std_logic_vector(3 downto 0);
signal dout              : std_logic_vector(7 downto 0);
signal add               : std_logic_vector(6 downto 0);
signal func_set_times    : integer;
signal busy_cnt          : integer;

begin

process(clk, rst)
begin
    if(rst = '1') then
        lcd_int_state <= IDLE;
        lcd_int_cnt <= 0;
        data <= x"ff";
    elsif(clk'event and clk = '1') then
        case lcd_int_state is
            when IDLE =>
                if(one_trans_req = '1') then
                    lcd_int_state <= RS_RW1_STATE;
                else
                    lcd_int_state <= IDLE;
                end if;
                data <= x"ff";
            when RS_RW1_STATE =>
                rs <= rs_pre;
                rw <= rw_pre;
                if(lcd_int_cnt = 4) then
                    lcd_int_state <= E1_STATE;
                    lcd_int_cnt <= 0;
                else
                    lcd_int_cnt <= lcd_int_cnt + 1;
                end if;
                if(read_write = '0') then
                    if(write_data_trans = '1') then
                        db_out <= dout;
                    else
                        db_out <= db_pre(7 downto 0);
                    end if;
                end if;
            when E1_STATE =>
                e <= '1';
                if(read_write = '0') then
                    lcd_int_state <= DB_STATE;
                else
                    if(lcd_int_cnt = 25) then
                        data <= db_in;
                    end if;
                    if(lcd_int_cnt = 30) then
                        lcd_int_state <= E2_STATE;
                        lcd_int_cnt <= 0;
                    else
                        lcd_int_cnt <= lcd_int_cnt + 1;
                    end if;
                end if;
            when DB_STATE =>
                if(lcd_int_cnt = 12) then
                    lcd_int_cnt <= 0;
                    lcd_int_state <= E2_STATE;
                else
                    lcd_int_cnt <= lcd_int_cnt + 1;
                end if;
            when E2_STATE =>
                e <= '0';
                if(lcd_int_cnt = 2) then
                    lcd_int_cnt <= 0;
                    lcd_int_state <= RS_RW2_STATE;
                else
                    lcd_int_cnt <= lcd_int_cnt + 1;
                end if;
            when RS_RW2_STATE =>
                rs <= '0';
                rw <= '0';
                lcd_int_state <= DONE;
            when DONE =>
                lcd_int_state <= IDLE;
            when OTHERS => lcd_int_state <= IDLE;
        end case;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        busy_flag <= '1';
    elsif(clk'event and clk = '1') then
        if(lcd_seq_state = CHECK_BUSY_FLAG) then
            if(lcd_int_state = E1_STATE and lcd_int_cnt = 26 and read_write = '1') then
                busy_flag <= data(7);
            end if;
        else
            busy_flag <= '1';
        end if;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        rs_pre <= '0';
        rw_pre <= '0';
    elsif(clk'event and clk = '1') then
        case cmd_type is
            when CLR_SCR =>
                rs_pre <= '0';
                rw_pre <= '0';
            when RT_HOME =>
                rs_pre <= '0';
                rw_pre <= '0';
            when MODE =>
                rs_pre <= '0';
                rw_pre <= '0';
            when DISP =>
                rs_pre <= '0';
                rw_pre <= '0';
            when FUNC =>
                rs_pre <= '0';
                rw_pre <= '0';
            when DDRAM_ADDR =>
                rs_pre <= '0';
                rw_pre <= '0';
            when BUSY_F =>
                rs_pre <= '0';
                rw_pre <= '1';
            when WRITE_DATA =>
                rs_pre <= '1';
                rw_pre <= '0';
            when others =>
                rs_pre <= rs_pre;
                rw_pre <= rw_pre;
        end case;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        db_pre <= (others => '0');
    elsif(clk'event and clk = '1') then
        case cmd_type is
            when CLR_SCR =>
                db_pre(7 downto 0) <= x"01";
            when RT_HOME =>
                db_pre(7 downto 0) <= x"02";
            when MODE =>
                db_pre(7 downto 0) <= x"06";
            when DISP =>
                db_pre(7 downto 0) <= x"0f";
            when FUNC =>
                db_pre(7 downto 0) <= x"3c";
                --db_pre(7 downto 0) <= x"38";
            when DDRAM_ADDR =>
                db_pre(7 downto 0) <= "1" & add;
            when WRITE_DATA =>
                case data_no is
                    when 0 => db_pre <= data0;
                    when 1 => db_pre <= data1;
                    when 2 => db_pre <= data2;
                    when 3 => db_pre <= data3;
                    when others => db_pre <= (others => '0');
                end case;
            when others =>
                db_pre <= db_pre;
        end case;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        db_out_en <= '1';
    elsif(clk'event and clk = '1') then
        if(read_write = '1') then
            db_out_en <= '0';
        else
            db_out_en <= '1';
        end if;
    end if;
end process;

process(db_pre, sel_pre)
begin 
    case sel_pre is
        when 0 => dout_tmp <= db_pre(31 downto 28);
        when 1 => dout_tmp <= db_pre(27 downto 24);
        when 2 => dout_tmp <= db_pre(23 downto 20);
        when 3 => dout_tmp <= db_pre(19 downto 16);
        when 4 => dout_tmp <= db_pre(15 downto 12);
        when 5 => dout_tmp <= db_pre(11 downto 8);
        when 6 => dout_tmp <= db_pre(7 downto 4);
        when 7 => dout_tmp <= db_pre(3 downto 0);
        when others => dout_tmp <= db_pre(3 downto 0);
    end case;
end process;

process(dout_tmp)
begin
    case dout_tmp is
        when x"0" => dout <= "00110000";
        when x"1" => dout <= "00110001";
        when x"2" => dout <= "00110010";
        when x"3" => dout <= "00110011";
        when x"4" => dout <= "00110100";
        when x"5" => dout <= "00110101";
        when x"6" => dout <= "00110110";
        when x"7" => dout <= "00110111";
        when x"8" => dout <= "00111000";
        when x"9" => dout <= "00111001";
        when x"a" => dout <= "01000001";
        when x"b" => dout <= "01000010";
        when x"c" => dout <= "01000011";
        when x"d" => dout <= "01000100";
        when x"e" => dout <= "01000101";
        when x"f" => dout <= "01000110";
        when others => dout <= "00000000";
    end case;
end process;


process(clk, rst)
begin
    if(rst = '1') then
        one_trans_req <= '0';
    elsif(clk'event and clk = '1') then
        if(cmd_type /= NONE) then
            one_trans_req <= '1';
        else
            one_trans_req <= '0';
        end if;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        write_data_trans <= '0';
    elsif(clk'event and clk = '1') then
        if(cmd_type = WRITE_DATA) then
            write_data_trans <= '1';
        elsif(cmd_type /= NONE) then
            write_data_trans <= '0';
        end if;
    end if;
end process;

process(clk, rst)
begin
    if(rst = '1') then
        lcd_seq_state_nxt <= LCD_INIT;
        lcd_seq_state <= LCD_INIT;
        cmd_type <= NONE;
        lcd_seq_cnt <= 0;
        read_write <= '0';
        data_no <= 0;
        sel <= 0;
        sel_pre <= 0;
        add <= "0000000";
        func_set_times <= 0;
        busy_cnt <= 0;
    elsif(clk'event and clk = '1') then
        case lcd_seq_state is
            when LCD_INIT => 
                if(lcd_seq_cnt = 3000000) then
                --if(lcd_seq_cnt = 20) then
                    lcd_seq_state <= FUNC_SET;
                end if;
                if(lcd_seq_cnt = 3000000) then
                --if(lcd_seq_cnt = 20) then
                    lcd_seq_cnt <= 0;
                else
                    lcd_seq_cnt <= lcd_seq_cnt + 1;
                end if;
            when FUNC_SET =>
                if(lcd_seq_cnt = 0) then
                    cmd_type <= FUNC;
                else
                    cmd_type <= NONE;
                end if;
                if(lcd_seq_cnt = 300000) then
                --if(lcd_seq_cnt = 2000) then
                    lcd_seq_cnt <= 0;
                    if(func_set_times = 2) then
                        lcd_seq_state <= DISP_ON;
                        func_set_times <= 0;
                    else
                        func_set_times <= func_set_times + 1;
                    end if;
                else
                    lcd_seq_cnt <= lcd_seq_cnt + 1;
                end if;
            when DISP_ON =>
                if(lcd_seq_cnt = 0) then
                    cmd_type <= DISP;
                else
                    cmd_type <= NONE;
                end if;
                if(lcd_seq_cnt = 2000) then
                    lcd_seq_state <= MODE_SET;
                    lcd_seq_cnt <= 0;
                else
                    lcd_seq_cnt <= lcd_seq_cnt + 1;
                end if;
            when MODE_SET =>
                if(lcd_seq_cnt = 0) then
                    cmd_type <= MODE;
                else
                    cmd_type <= NONE;
                end if;
                if(lcd_seq_cnt = 2000) then
                    lcd_seq_state <= CLEAR_SCR;
                    lcd_seq_cnt <= 0;
                else
                    lcd_seq_cnt <= lcd_seq_cnt + 1;
                end if;
            when CLEAR_SCR =>
                cmd_type <= CLR_SCR;
                --lcd_seq_state_nxt <= REFRESH_STATE;
                lcd_seq_state_nxt <= RET_HOME;
                lcd_seq_state <= CHECK_BUSY_FLAG;
            --when REFRESH_STATE =>
            --    if(refresh = '1') then
            --        lcd_seq_state <= RET_HOME;
            --    end if;
            when RET_HOME =>
                cmd_type <= RT_HOME;
                lcd_seq_state_nxt <= ADDR0_SET;
                lcd_seq_state <= CHECK_BUSY_FLAG;
            when ADDR0_SET =>
                cmd_type <= DDRAM_ADDR;
                add <= "0000000";
                lcd_seq_state_nxt <= WRITE_DATA0;
                lcd_seq_state <= CHECK_BUSY_FLAG;
            when WRITE_DATA0 =>
                cmd_type <= WRITE_DATA;
                if(data_no = 1 and sel = 8) then
                    lcd_seq_state_nxt <= ADDR1_SET;
                    sel <= 0;
                    data_no <= 2;
                else
                    if(sel < 8) then
                        sel <= sel + 1;
                    else
                        sel <= 1;
                        data_no <= data_no + 1;
                    end if;
                    lcd_seq_state_nxt <= WRITE_DATA0;
                end if;
                if(sel = 8) then
                    sel_pre <= 0;
                else
                    sel_pre <= sel;
                end if;
                lcd_seq_state <= CHECK_BUSY_FLAG;

                --if(lcd_seq_cnt = 0) then
                --    lcd_seq_cnt <= 0;
                --    sel <= 0;
                --    lcd_seq_state_nxt <= REFRESH_STATE;
                --else
                --    lcd_seq_cnt <= lcd_seq_cnt + 1;
                --    sel <= sel + 1;
                --    lcd_seq_state_nxt <= WRITE_DATA0;
                --end if;
                --sel_pre <= sel;
                --lcd_seq_state <= CHECK_BUSY_FLAG;
            when ADDR1_SET =>
                cmd_type <= DDRAM_ADDR;
                add <= "1000000";
                lcd_seq_state_nxt <= WRITE_DATA1;
                lcd_seq_state <= CHECK_BUSY_FLAG;
            when WRITE_DATA1 =>
                cmd_type <= WRITE_DATA;
                if(data_no = 3 and sel = 8) then
                --if(data_no = 1 and sel = 7) then
                    --lcd_seq_state_nxt <= REFRESH_STATE;
                    lcd_seq_state_nxt <= RET_HOME;
                    data_no <= 0;
                    sel <= 0;
                else
                    if(sel < 8) then
                        sel <= sel + 1;
                    else
                        sel <= 1;
                        data_no <= data_no + 1;
                    end if;
                    lcd_seq_state_nxt <= WRITE_DATA1;
                end if;
                if(sel = 8) then
                    sel_pre <= 0;
                else
                    sel_pre <= sel;
                end if;
                lcd_seq_state <= CHECK_BUSY_FLAG;

                --if(lcd_seq_cnt = 3) then
                --    lcd_seq_cnt <= 0;
                --    sel <= 0;
                --    lcd_seq_state_nxt <= REFRESH_STATE;
                --else
                --    lcd_seq_cnt <= lcd_seq_cnt + 1;
                --    sel <= sel + 1;
                --    lcd_seq_state_nxt <= WRITE_DATA1;
                --end if;
                --sel_pre <= sel;
                --lcd_seq_state <= CHECK_BUSY_FLAG;
            when CHECK_BUSY_FLAG =>
                if(busy_cnt < 2000) then
                    busy_cnt <= busy_cnt + 1;
                    cmd_type <= NONE;
                else
                    busy_cnt <= 0;
                    cmd_type <= NONE;
                    lcd_seq_state <= lcd_seq_state_nxt;
                    read_write <= '0';
                end if;
            when others =>
                lcd_seq_state <= LCD_INIT;
        end case;
    end if;
end process;

end behavioral;

